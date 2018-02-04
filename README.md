PasPandoc is a [pandoc](http://pandoc.org) wrapper written in Pascal (for
[free pascal](https://www.freepascal.org/)).

License: GPLv3

Requirements: [free pascal](https://www.freepascal.org/) >= 3.1 because it
uses the new generics classes.

# Example

Convert a markdown input file to a PDF file (see `examples/HelloWorldToPDF`):

```Pascal
program HelloWorld;
{$mode objfpc}{$H+}{$J-}
{$UNITPATH ../../src}
uses PandocConverter;

var
    Pandoc: TPandocConverter;
begin
    { Create a new Pandoc converter }
    Pandoc := TPandocConverter.Create();
    
    { Use single command-line options by assigning a String, Boolean, or
      Integer value to its property. }
    Pandoc.ReadFrom := 'markdown';
    Pandoc.WriteTo := 'latex';
    Pandoc.TableOfContents := True;
    Pandoc.TocDepth := 3;
    Pandoc.NumberSections := True;

    { You can assign a string to a multiple command-line option multiple
      times. It will added to a list of values for that property. }
    Pandoc.Metadata := 'title=PasPandoc';
    Pandoc.Metadata := 'author=Huub de Beer';

    { Command-line options that take a value from an enumeration as value are
    typed: the values of the enumeration are formed by taking the first two
    characters of the property name followed by the pandoc enumeration value.  }
    Pandoc.PDFEngine := pdPDFLatex;
    Pandoc.Output := 'hello.pdf';

    { Convert an input file. }
    Pandoc.ConvertFile('hello.md');
end.
```
