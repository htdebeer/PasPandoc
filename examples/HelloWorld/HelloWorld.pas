program HelloWorld;
{$mode objfpc}{$H+}{$J-}
{$UNITPATH ../../src}
uses PandocConverter;

var
    Pandoc: TPandocConverter;
begin
    { Create a Pandoc converter }
    Pandoc := TPandocConverter.Create();
    { Setup the converter with pandoc command-line options turned to properties }
    Pandoc.ReadFrom := 'markdown';
    Pandoc.WriteTo := 'html';
    { Convert a markdown string to HTML and write it to STDOUT }
    WriteLn(Pandoc.Convert('hello **world**'));
end.
