# Thread Dump Analyzer

This project was an excuse to make an application of the Scala JS stack :o)

The aim for this project is to build a standalone tool that produces, in a web browser, analytic data based on a provided Java thread dump.
The rendering phase is based on Twitter Bootstrap. 

To use it, just run :
 
 ```
 $ sbt fastOptJS  
 $ bower install
 ```
 
 and serve the test.html page.


## Release notes

### Version 2
- Expand all / Collapse All feature
- Link to Grepcode
- No more empty stack display for system threads

### Version 3
- Bug with collapse and Bootstrap 3.0.3
- Button formatting