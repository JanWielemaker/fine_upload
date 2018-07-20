# SWI-Prolog backend for Fine File Uploader

This pack provides a SWI-Prolog backend   for  uploading files from your
browser       to       a       Prolog       HTTP       server.       The
[FineUploader](https://fineuploader.com/)  is  a    feature-rich  client
library for uploading files. This  package   provides  the backend and a
demo program that shows basic usage.

## Installation

```{prolog}
?- pack_install(fine_upload).
```

## Demo

Go to the package directory (typically `<swihome>/pack/fine_upload`) and
run the code below. Then direct your browser to http://localhost:8080/

```
$ swipl demo/upload.pl
?- run.
% Started server at http://localhost:8080/
```


