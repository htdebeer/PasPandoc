program HelloWorld;
{$mode objfpc}{$H+}{$J-}
{$UNITPATH ../../src}
uses PandocConverter, PandocFilter;

var
    Pandoc: TPandocConverter;
    Document: TPandocDocument;
    DocumentJSONString: String;
begin
    Pandoc := TPandocConverter.Create;
    Pandoc.ReadFrom := 'markdown';
    Pandoc.WriteTo := 'json';
    DocumentJSONString := Pandoc.Convert('Hello world');

    Document := TPandocDocument.Create(DocumentJSONString);

    WriteLn(Document.ToJSON);
end.
