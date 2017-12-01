program HelloWorld;
{$mode objfpc}{$H+}{$J-}
{$UNITPATH ../../src}
uses PandocConverter;

var
    Pandoc: TPandocConverter;
begin
    Pandoc := TPandocConverter.Create();
    Pandoc.ReadFrom := 'markdown';
    Pandoc.WriteTo := 'html';
    WriteLn(Pandoc.Convert('hello **world**'));
end.
