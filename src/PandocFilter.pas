{

    Copyright 2017, 2018 Huub de Beer <Huub@heerdebeer.org>
    
    This file is part of PasPandoc.

    PasPandoc is free software: you can redistribute it and/or modify it under
    the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    PasPandoc is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with PasPandoc.  If not, see <http://www.gnu.org/licenses/>.

}
{


}
unit PandocFilter;
{$mode objfpc}{$H+}{$J-}

interface

uses 
    Classes,
    Generics.Collections,
    SysUtils,
    fpjson, 
    jsonparser
    ;

const
    PANDOC_TYPES_API_VERSION = 1;
    PANDOC_TYPES_MAJOR_VERSION = 17;
    PANDOC_TYPES_MINOR_VERSION = 3;

type

    TVersionParts = (vAPI, vMajor, vMinor);
    TVersion = Array [TVersionParts] of Integer;

    { 
    
    Pandoc AST node types as defined in https://hackage.haskell.org/package/pandoc-types-1.17.3/docs/Text-Pandoc-Definition.html 
    
    }
    TPandocASTNodeType = (
        { Meta types }
        ptMetaMap,
        ptMetaList,
        ptMetaBool,
        ptMetaString,
        ptMetaInlines,
        ptMetaBlocks,
        { Block types }
        ptPlain,
        ptPara,
        ptLineBlock,
        ptCodeBlock,
        ptRawBlock,
        ptBlockQuote,
        ptOrderedList,
        ptBulletList,
        ptDefinitionList,
        ptHeader,
        ptHorizontalRule,
        ptTable,
        ptDiv,
        ptNull,
        { Inline types }
        ptStr,
        ptEmph,
        ptStrong,
        ptStrikeout,
        ptSuperscript,
        ptSubscript,
        ptSmallCaps,
        ptQuoted,
        ptCite,
        ptCode,
        ptSpace,
        ptSoftBreak,
        ptLineBreak,
        ptMath,
        ptRawInline,
        ptLink,
        ptImage,
        ptNote,
        ptSpan
    );
    TMetaType = ptMetaMap .. ptMetaBlocks;
    TBlockType = ptPlain .. ptNull;
    TInlineType = ptStr .. ptSpan;


    TPandocNode = class;
    TPandocNodeList = specialize TList<TPandocNode>;
    TPandocNodeMap = specialize TDictionary<String, TPandocNode>;

    TPandocNode = class
    private
        FType: TPandocASTNodeType;
    public
        constructor Create(ASTType: TPandocASTNodeType);
        constructor Create(AST: TJSONObject);
        destructor Destroy; override;

        function ToJSON: TJSONObject;

        property ASTType: TPandocASTNodeType read FType;
    end;

    TBlockNode = class (TPandocNode) 
    private
        FContents: TPandocNodeList;
    public
        constructor Create(ASTType: TBlockType); override;
        constructor Create(AST: TJSONObject); override;
        destructor Destroy; override;

        function ToJSON: TJSONObject; override;

        property Contents: TPandocNodeList read FContents write FContents;
    end;

    TInlineNode = class (TPandocNode)
    private
        FContents: TPandocNodeList;
    public
        constructor Create(ASTType: TInlineType); override;
        constructor Create(AST: TJSONObject); override;
        destructor Destroy; override;

        function ToJSON: TJSONObject; override;

        property Contents: TPandocNodeList read FContents write FContents;
    end;

    TPara = class(TBlockNode)
    public
        constructor Create();
        constructor Create(AST: TJSONObject); overload;

        function ToJSON: overload;
    end;

    TStr = class(TInlineNode)
    private
        FContents: String;
    public
        constructor Create();
        constructor Create(AST: TJSONObject); override;
        destructor Destroy; override;

        function ToJSON: TJSONObject; override;

        property Contents: String read FContents write FContents;
    end;

    TPandocMetadata = class
    private
        FMetadata: TPandocNodeMap;
    public
        constructor Create;
        constructor Create(AST: TJSONObject);
        destructor Destroy; override;

        function ToJSON: TJSONObject;
    end;
    
    TPandocDocument = class
    private
        FVersion: TVersion;
        FBlocks: TPandocNodeList;
        FMetadata: TPandocMetadata;

        procedure SetVersion(VersionArray: TJSONArray);
    public
        { Create an empty pandoc document }
        constructor Create();
        { Create a pandoc document from a JSON representation of a pandoc AST }
        constructor Create(JSONString: String);
        destructor Destroy; override;

        { Convert this document to a JSON representation of a pandoc AST }
        function ToJSON: String; 
        function GetVersion: String;

        property Version: TVersion read FVersion write FVersion;
        property Blocks: TPandocNodeList read FBlocks write FBlocks;
        property Metadata: TPandocMetadata read FMetadata write FMetadata;
    end;

    
function PandocASTNodeTypeToString(ASTNodeType: TPandocASTNodeType): String;
function StringToPandocASTNodeType(ASTNodeType: String): TPandocASTNodeType;

implementation

const 

    PANDOC_API_VERSION_KEY = 'pandoc-api-version';
    META_KEY = 'meta';
    BLOCKS_KEY = 'blocks';
    TYPE_KEY = 't';
    CONTENTS_KEY = 'c';


function PandocASTNodeTypeToString(ASTNodeType: TPandocASTNodeType): String;
begin
    WriteStr(Result, ASTNodeType);
    { Strip 'pt' from the string }
    Result := Copy(Result, 3, Length(Result) - 2);
end;

function StringToPandocASTNodeType(ASTNodeType: String): TPandocASTNodeType;
var
    ASTNodeTypeString: String;
begin
    ASTNodeTypeString := 'pt' + ASTNodeType;
    ReadStr(ASTNodeTypeString, Result);
end;

{ 
*******************************
TPandocNode
*******************************
}
constructor TPandocNode.Create(ASTType: TPandocASTNodeType);
begin
    FType := ASTType;
    FContents := TPandocNodeList.Create;
end;

constructor TPandocNode.Create(AST: TJSONObject);
var
    ContentItem: TJSONEnum;
    ContentObject: TJSONObject;

    TypeString: String;
    ContentsArray: TJSONArray;
begin
    TypeString := AST.Get(TYPE_KEY, '');
    Create(StringToPandocASTNodeType(TypeString));

    ContentsArray := TJSONArray(AST.Get(CONTENTS_KEY, TJSONArray.Create));
    for ContentItem in ContentsArray do
    begin
        ContentObject := TJSONObject(ContentItem.Value);
        FContents.Add(TPandocNode.Create(ContentObject));
    end
end;

destructor TPandocNode.Destroy;
begin;
    FreeAndNil(FType);
    FreeAndNil(FContents);
    inherited;
end;

function TPandocNode.ToJSON: TJSONObject;
var
    ContentsArray: TJSONArray;
    ContentNode: TPandocNode;
begin
    Result := TJSONObject.Create;
    Result.Add(TYPE_KEY, PandocASTNodeTypeToString(FType));
    ContentsArray := TJSONArray.Create;
    for ContentNode in FContents do
    begin
        ContentsArray.Add(ContentNode.ToJSON);
    end;
    Result.Add(CONTENTS_KEY, ContentsArray);
end;

{ 
*******************************
TPandocMetadata
*******************************
}
constructor TPandocMetadata.Create;
begin
    FMetadata := TPandocNodeMap.Create;
end;

constructor TPandocMetadata.Create(AST: TJSONObject);
begin
end;

destructor TPandocMetadata.Destroy;
begin;
    inherited;
end;

function TPandocMetadata.ToJSON: TJSONObject;
begin
    Result := TJSONObject.Create;
end;

{ 
*******************************
TPandocDocument
*******************************
}
constructor TPandocDocument.Create;
begin
    FVersion[vAPI] := PANDOC_TYPES_API_VERSION;
    FVersion[vMajor] := PANDOC_TYPES_MAJOR_VERSION;
    FVersion[vMinor] := PANDOC_TYPES_MINOR_VERSION;
    FBlocks := TPandocNodeList.Create;
    FMetadata := TPandocMetadata.Create;
end;

constructor TPandocDocument.Create(JSONString: String);
var
    AST: TJSONData;
    VersionArray: TJSONArray;
    BlocksArray: TJSONArray;
    BlockObject: TJSONObject;
    MetaObject: TJSONObject;

    JSONBlock: TJSONEnum;
    BlockNode: TPandocNode;
begin
    Create;
    AST := GetJSON(JSONString);

    VersionArray := TJSONArray(AST.GetPath(PANDOC_API_VERSION_KEY));
    SetVersion(VersionArray);

    BlocksArray := TJSONArray(AST.GetPath(BLOCKS_KEY));
    
    for JSONBlock in BlocksArray do
    begin
        BlockObject := TJSONObject(JSONBlock.Value);
        BlockNode := TPandocNode.Create(BlockObject);
        FBlocks.Add(BlockNode);
    end;

    MetaObject := TJSONObject(AST.GetPath(META_KEY));
    FMetadata := TPandocMetadata.Create(MetaObject);
end;

destructor TPandocDocument.Destroy;
begin
    FreeAndNil(FVersion);
    FreeAndNil(FBlocks);
    FreeAndNil(FMetadata);
    inherited;
end;

function TPandocDocument.ToJSON: String;
var
    DocObject: TJSONObject;
    VersionArray: TJSONArray;
    BlocksArray: TJSONArray;
    Block: TPandocNode;
    MetaObject: TJSONObject;
begin
    DocObject := TJSONObject.Create;
    
    VersionArray := TJSONArray.Create;
    VersionArray.Add(FVersion[vAPI]);
    VersionArray.Add(FVersion[vMajor]);
    VersionArray.Add(FVersion[vMinor]);

    DocObject.Add(PANDOC_API_VERSION_KEY, VersionArray);

    BlocksArray := TJSONArray.Create;
    for Block in FBlocks do
    begin
        BlocksArray.Add(Block.ToJSON);
    end;

    DocObject.Add(BLOCKS_KEY, BlocksArray);

    MetaObject := TJSONObject.Create;
    DocObject.Add(META_KEY, MetaObject);

    Result := DocObject.AsJSON;
end;

procedure TPandocDocument.SetVersion(VersionArray: TJSONArray);
var
    VersionParts: TStringArray;
begin
    FVersion[vAPI] := VersionArray.Integers[0];
    FVersion[vMajor] := VersionArray.Integers[1];
    FVersion[vMinor] := VersionArray.Integers[2];
end;

function TPandocDocument.GetVersion: String;
begin
    Result := IntToStr(FVersion[vAPI]) + 
        '.' + IntToStr(FVersion[vMajor]) + 
        '.' + IntToStr(FVersion[vMinor]);
end;


end.
