/**
 *  SWI-Prolog fine uploader demo
 *
 *  Author: Jan Wielemaker <J.Wielemaker@vu.nl>
 *  License: This file is in the public domain
 */

:- module(fine_upload_demo,
          [ run/0
          ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(broadcast)).

:- use_module('../prolog/http/fine_uploader').

% Uncomment to see some progress debug.
% :- debug(upload).

/** <module> Fine file upload demo

This module demonstrates the Fine file  uploader integration. The server
backend is in `../prolog/http/fine_uploader.pl`.  This   file  builds an
example web page. We considered wrapping all  this in a library, but the
almost infinite number of  configuration   and  deployment options makes
this a doubtful enterprice.

This file is placed in  the  public   domain  such  that  you can freely
copy/paste from it. Note that the notably the style and template HTML is
subject to the fine uploader license.

@see https://docs.fineuploader.com/endpoint_handlers/traditional.html
*/

%!  run
%
%   start the server at port 8080

run :-
    http_server(http_dispatch, [port(8080)]).

%!  handle_upload(+File, +Path, +Request) is det.
%
%   The uploading of a file was completed. The   file is in Path and was
%   named File on the user's machine. Request is the full HTTP request.


:- listen(file_upload(File, Into, Request),
          handle_upload(File, Into, Request)).

handle_upload(File, Into, Request) :-
    size_file(Into, Size),
    option(peer(Peer), Request),
    format(user_error,
           "File ~p has been uploaded to ~p (~D bytes from ~p)~n",
           [File, Into, Size, Peer]).


% Define the JS and CSS as  a   resource  to simplify including it using
% html_requires//1.

:- html_resource(
       fine_uploader,
       [ virtual(true),
         requires([ js('fine-uploader.js'),
                    css('fine-uploader-new.css')
                  ])
       ]).


:- http_handler(root(.), home, []).

%!  home(+Request)
%
%   Emit the demo welcome page at /

home(_Request) :-
    reply_html_page(
        title('Fine Uploader -- SWI-Prolog backend'),
        [ h1('Fine Uploader -- SWI-Prolog backend'),
          \fine_uploader
        ]).

%!  fine_uploader//
%
%   Emit the file uploader.  This  consists   of  the  header  elements,
%   template, style, the uploader itself  and   finally  the script that
%   combines it all.

fine_uploader -->
    html_requires(fine_uploader),
    fine_template,
    fine_style,

    html(div(id('fine-uploader-manual-trigger'), [])),

    fine_script.

%!  fine_script//
%
%   Emit the script that links all parts   together.  Note that we first
%   use  http_link_to_id/3  and  http_absolute_location/3  to  find  the
%   endpoint locations for this service.

fine_script -->
    { http_link_to_id(fine_uploads, [], UploadURL),
      http_link_to_id(fine_upload_finished, [], FinishedURL),
      http_link_to_id(fine_upload_delete, [], DeleteURL),
      http_absolute_location(icons(.), IconURL, [])
    },

    js_script({|javascript(UploadURL,FinishedURL,DeleteURL,IconURL)
              ||
var manualUploader = new qq.FineUploader({
    element: document.getElementById('fine-uploader-manual-trigger'),
    template: 'qq-template-manual-trigger',
    request: {
        endpoint: UploadURL
    },
    thumbnails: {
        placeholders: {
            waitingPath: IconURL+'/placeholders/waiting-generic.png',
            notAvailablePath: IconURL+'/placeholders/not_available-generic.png'
        }
    },
    deleteFile: {
        enabled: true,
        endpoint: DeleteURL,
        method: 'DELETE'
    },
    chunking: {
        enabled: true,
        success: {
            endpoint: FinishedURL,
            jsonPayload: true
        }
    },
    autoUpload: false,
    debug: true,
    folders: true,
    validation: {
      allowedExtensions: ["pdf", "html", "jpg", "jpeg", "png"],
      acceptFiles: "application/pdf,text/html,image/jpeg,image/png"
    }
});

qq(document.getElementById("trigger-upload")).attach("click", function() {
    manualUploader.uploadStoredFiles();
});
              |}).


%!  fine_style//
%
%   Define additional styles

fine_style -->
    html({|html||
<style>
    #trigger-upload {
        color: white;
        background-color: #00ABC7;
        font-size: 14px;
        padding: 7px 20px;
        background-image: none;
    }

    #fine-uploader-manual-trigger .qq-upload-button {
        margin-right: 15px;
    }

    #fine-uploader-manual-trigger .buttons {
        width: 36%;
    }

    #fine-uploader-manual-trigger .qq-uploader .qq-total-progress-bar-container {
        width: 60%;
    }
</style>
         |}).


		 /*******************************
		 *      TEMPLATE AND FILES	*
		 *******************************/

%!  fine_template//
%
%   Emit the template for the file uploader. Tried to use a `src` in the
%   script, but that only seems to  work for JavaScript script elements.
%   We now read the script and inject it directly.

fine_template -->
    { read_file_to_string(template('fine-uploader.html'), Template, []) },
    html(script([ type('text/template'),
                  id('qq-template-manual-trigger')
                ], \[Template])).

% Configure locations of the files where we can find web resources

:- multifile
    http:location/3.                % Alias, Expansion, Options
:- dynamic
    http:location/3.                % Alias, Expansion, Options

http:location(template, root(template), []).

user:file_search_path(web,      web).
user:file_search_path(template, web(template)).
user:file_search_path(js,       web(js)).
user:file_search_path(css,      web(css)).
user:file_search_path(icons,    web(icons)).

:- http_handler(template(.), serve_files_in_directory(template),
                [prefix]).
