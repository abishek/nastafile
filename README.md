# nastafile

A NNTP Archive to HTML generator. 

## Getting Started
You'll need to get [Qlot](https://github.com/fukamachi/qlot) first.

To install dependencies, do in the command-line:

```
qlot install
```


## TODO

* Fix 1632 'Unable to parse email body' errors.
* Add a command line utility to update archive.
* Add progress bars.
* Save the state of the already processed emails
  to process only a newer messages.
* Make code tunable.
* Support images from emails. As example we may take email with subject
  "Setting bigger font for a button" (email number 15449) from the gmane.lisp.lispworks.general. 
* Article 14370 can't be rendered because of this error in the (subject a) call:

  ```
  :GB18030 is not a valid encoding designator
  ```
