/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(file_uploader, []).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(filesex)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(debug)).
:- use_module(library(broadcast)).

:- debug(upload).

/** <module> Fine uploader HTTP backend

This module implements the _traditional_  HTTP   backend  methods to use
_Fine Uploader_, a modern and  feature-rich   web  client  for uploading
files to an HTTP server.  The   upload  backend implements the following
features:

  - Support one-shot uploads as well as chunked uploads.
  - Support deletion.

Files that are received are placed  in   a  directory  controlled by the
setting  `upload_directory`,  which  defaults  to    `uploads`.  If  the
directory does not exsit but can be created this is done.

Integration with the remainder of the   server is accomplished using the
library(broadcast). This library broadcasts the following events:

  - file_upload(+Name, +SavedFile, +Request)
    A file named Name has been uploaded to SavedFile.  SavedFile is
    an absolute path to the file saved in the directory defined by
    the setting `upload_directory` and has the same extension as the
    original file.  Request may be used to identify the user.
    This hook is executed in the context of the receiving HTTP worker.

@see https://docs.fineuploader.com/endpoint_handlers/traditional.html
*/

:- setting(upload_directory, atom, uploads,
           "Directory for storing uploaded files").
:- setting(upload_size_limit, nonneg, 100 000 000,
           "Max size for an uploaded file").

http:location(fine, root(fine), []).

:- http_handler(fine(uploads),           fine_uploads,
                [method(post)]).
:- http_handler(fine(uploads/finished),  fine_upload_finished,
                [method(post)]).
:- http_handler(fine('uploads/delete/'), fine_upload_delete,
                [method(delete), prefix]).


		 /*******************************
		 *          HTTP METHODS	*
		 *******************************/

%!  fine_uploads(+Request)
%
%   Handle a _Fine Uploader_ upload request.   This currently deals with
%   both one-shot and chunked uploading. If   an upload is completed the
%   system will broadcast the following message:
%
%       file_upload(Name, SavedFile, Request)
%
%   @see https://docs.fineuploader.com/endpoint_handlers/traditional.html

fine_uploads(Request) :-
    is_multipart_post_request(Request),
    catch(( http_read_data(Request, Parts,
                           [ on_filename(save_file(Request, Parts))
                           ]),
            memberchk(qqfile=file(FileName, Saved), Parts),
            debug(upload, 'Saved ~p in ~p', [FileName, Saved])
          ), E, true),
    !,
    (   var(E)
    ->  reply_json_dict(json{success: true})
    ;   message_to_string(E, Msg),
        reply_json_dict(json{error: Msg})
    ).
fine_uploads(_Request) :-
    reply_json_dict(json{error: "Bad file upload request"}).

is_multipart_post_request(Request) :-
    memberchk(method(post), Request),
    memberchk(content_type(ContentType), Request),
    http_parse_header_value(
        content_type, ContentType,
        media(multipart/'form-data', _)).

:- public save_file/5.

save_file(Request, Parts0, In, file(FileName, Path), Options) :-
    copy_term(Parts0, Parts),
    once(append(Parts, [], _)),                 % close the list
    debug(upload, 'Params so far: ~p', [Parts]),
    (   option(filename(FileName), Options),
        FileName \== blob
    ->  true
    ;   part(qqfilename, Parts, FileName)
    ),
    file_name_extension(_, Ext, FileName),
    part(qquuid, Parts, UUID),
    upload_file(UUID, Ext, Dir, Path),
    enforce_file_size_limit(Parts, FileName, Size),
    part_offset(Parts, Size, IsMulti, Offset),
    debug(upload, 'IsMulti = ~p, Offset = ~p', [IsMulti, Offset]),
    make_directory_path(Dir),
    setup_call_cleanup(
        open(Path, write, Out,
             [ type(binary)
             ]),
        (   seek(Out, Offset, bof, NewOffset),
            assertion(Offset == NewOffset),
            copy_stream_data(In, Out)
        ),
        close(Out)),
    (   IsMulti == false
    ->  broadcast(file_upload(FileName, Path, Request))
    ;   true
    ).

enforce_file_size_limit(Parts, FileName, Size) :-
    part(qqtotalfilesize, Parts, Size),
    setting(upload_size_limit, Limit),
    (   Size =< Limit
    ->  true
    ;   permission_error(upload, file, FileName)
    ).

part_offset(Parts, Size, true, Offset) :-
    part(qqpartbyteoffset, Parts, Offset),
    (   Offset =< Size
    ->  true
    ;   permission_error(offset, file, Offset)
    ),
    !.
part_offset(_, _, false, 0).

part(Name, Parts, Value) :-
    memberchk(Name=V0, Parts),
    (   atom_number(V0, Value)
    ->  true
    ;   Value = V0
    ).

%!  fine_upload_finished(+Request)
%
%   Called after all parts for a file are transferred

fine_upload_finished(Request) :-
    http_read_json_dict(Request, Parts),
    debug(upload, 'Finished params: ~p', [Parts]),
    upload_file(Parts.qquuid, _Ext, _Dir, SavedFile),
    broadcast(file_upload(Parts.qqfilename, SavedFile, Request)),
    reply_json_dict(json{success: true}).

upload_file(UUID, Ext, Dir, Path) :-
    nonvar(Ext),
    !,
    setting(upload_directory, Dir),
    file_name_extension(UUID, Ext, File),
    directory_file_path(Dir, File, Path).
upload_file(UUID, Ext, Dir, Path) :-
    atom_concat(UUID, '.*', FilePattern),
    setting(upload_directory, Dir),
    directory_file_path(Dir, FilePattern, Pattern),
    expand_file_name(Pattern, [Path]),
    file_name_extension(_, Ext, Path).

%!  fine_upload_delete(+Request)
%
%   Handle a DELETE request. The argument is  a UUID that should only be
%   known to the client, so this should be safe.

fine_upload_delete(Request) :-
    option(path_info(UUID), Request),
    upload_file(UUID, _Ext, _Dir, Path),
    delete_file(Path),
    reply_json_dict(json{success: true}).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(bad_file_upload) -->
    [ 'A file upload must be submitted as multipart/form-data using', nl,
      'name=file and providing a file-name'
    ].
