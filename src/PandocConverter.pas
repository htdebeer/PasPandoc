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

    The PandocConverter unit wraps the pandoc program in a convenient to use
    class: TPandocConverter. After creating an instance of TPandocConverter,
    you can specify options to the converter by assigning values to its
    properties. Once you have setup the converter, run the actual conversion
    by means of the Convert, ConvertFile, or ConvertFiles methods.

}
unit PandocConverter;
{$mode objfpc}{$H+}{$J-}

interface

uses 
    Generics.Collections,
    Classes,
    Process
    ;

type
    { 

        A base class to represent pandoc's command-line options. Each
        command-line option has a name. Furthermore, command-line options can
        be added to the parameters of a TProcess.

    }
    TCommandLineOption = class
    private
        FName: String;
    public
        { 

            This command-line option's name. It can only be set through the
            constructor. 

        }
        property Name: String read FName;

        { 

            Create a new TCommandLineOption with its name.

        }
        constructor Create(OptionName: String);
        
        { 
        }
        procedure ToParameters(PandocProcess: TProcess; ValueString: String); overload;

        {
        }
        procedure ToParameters(PandocProcess: TProcess); virtual; abstract; overload;
    end;
    
    { 

        A Boolean command-line option or flag. It can be on or off. When it is off it
        will not be present as a parameter. 

    }
    TBooleanCommandLineOption = class(TCommandLineOption)
    private
        FValue: Boolean;
    public
        { 

            This command-line option's value. 

        }
        property Value: Boolean read FValue write FValue;

        {

            Create a new TBooleanCommandLineOption with a name and a value.

        }
        constructor Create(OptionName: String; OptionValue: Boolean);
        
        procedure ToParameters(PandocProcess: TProcess); override;
    end;

    TIntegerCommandLineOption = class(TCommandLineOption)
    private
        FValue: Integer;
    public
        property Value: Integer read FValue write FValue;
        constructor Create(OptionName: String; OptionValue: Integer);
        procedure ToParameters(PandocProcess: TProcess); override;
    end;

    TStringCommandLineOption = class(TCommandLineOption)
    private
        FValue: String;
    public
        property Value: String read FValue write FValue;
        constructor Create(OptionName: String; OptionValue: String);
        procedure ToParameters(PandocProcess: TProcess); override;
    end;

    TMultipleCommandLineOption = class(TCommandLineOption)
    private
        FValue: TStringList;
    public
        property Value: TStringList read FValue write FValue;
        constructor Create(OptionName: String; OptionValue: TStringList); overload;
        constructor Create(OptionName: String); overload;
        destructor Destroy(); override;

        procedure Push(OptionValue: String);
        procedure Remove(OptionValue: String);
        procedure Clear();

        procedure ToParameters(PandocProcess: TProcess); override;
    end;


    TTrackChanges = (
        trAccept,
        trReject,
        trAll
    );

    TTrackChangesCommandLineOption = class(TCommandLineOption)
    private
        FValue: TTrackChanges;
    public
        property Value: TTrackChanges read FValue write FValue;
        constructor Create(OptionName: String; OptionValue: TTrackChanges);
        procedure ToParameters(PandocProcess: TProcess); override;
    end;
    TEol = (
        eoCrlf,
        eoLf,
        eoNative
    );

    TEolCommandLineOption = class(TCommandLineOption)
    private
        FValue: TEol;
    public
        property Value: TEol read FValue write FValue;
        constructor Create(OptionName: String; OptionValue: TEol);
        procedure ToParameters(PandocProcess: TProcess); override;
    end;
    TWrap = (
        wrAuto,
        wrNone,
        wrPreserve
    );

    TWrapCommandLineOption = class(TCommandLineOption)
    private
        FValue: TWrap;
    public
        property Value: TWrap read FValue write FValue;
        constructor Create(OptionName: String; OptionValue: TWrap);
        procedure ToParameters(PandocProcess: TProcess); override;
    end;
    TReferenceLocation = (
        reBlock,
        reSection,
        reDocument
    );

    TReferenceLocationCommandLineOption = class(TCommandLineOption)
    private
        FValue: TReferenceLocation;
    public
        property Value: TReferenceLocation read FValue write FValue;
        constructor Create(OptionName: String; OptionValue: TReferenceLocation);
        procedure ToParameters(PandocProcess: TProcess); override;
    end;
    TTopLevelDivision = (
        toDefault,
        toSection,
        toChapter,
        toPart
    );

    TTopLevelDivisionCommandLineOption = class(TCommandLineOption)
    private
        FValue: TTopLevelDivision;
    public
        property Value: TTopLevelDivision read FValue write FValue;
        constructor Create(OptionName: String; OptionValue: TTopLevelDivision);
        procedure ToParameters(PandocProcess: TProcess); override;
    end;
    TEmailObfuscation = (
        emNone,
        emJavascript,
        emReferences
    );

    TEmailObfuscationCommandLineOption = class(TCommandLineOption)
    private
        FValue: TEmailObfuscation;
    public
        property Value: TEmailObfuscation read FValue write FValue;
        constructor Create(OptionName: String; OptionValue: TEmailObfuscation);
        procedure ToParameters(PandocProcess: TProcess); override;
    end;
    TPDFEngine = (
        pdPdflatex,
        pdLualatex,
        pdXelatex,
        pdWkhtmltopdf,
        pdWeasyprint,
        pdPrince,
        pdContext,
        pdPdfroff
    );

    TPDFEngineCommandLineOption = class(TCommandLineOption)
    private
        FValue: TPDFEngine;
    public
        property Value: TPDFEngine read FValue write FValue;
        constructor Create(OptionName: String; OptionValue: TPDFEngine);
        procedure ToParameters(PandocProcess: TProcess); override;
    end;

    TPandocOption = (
        poReadFrom,
        poWriteTo,
        poOutput,
        poDataDir,
        poBashCompletion,
        poVerbose,
        poQuiet,
        poFailIfWarnings,
        poLog,
        poListInputFormats,
        poListOutputFormats,
        poListExtensions,
        poListHighlightLanguages,
        poListHighlightStyles,
        poVersion,
        poHelp,
        poBaseHeaderLevel,
        poIndentedCodeClasses,
        poDefaultImageExtension,
        poFileScope,
        poFilter,
        poLuaFilter,
        poMetadata,
        poPreserveTabs,
        poTabStop,
        poTrackChanges,
        poExtractMedia,
        poAbbreviations,
        poStandalone,
        poTemplate,
        poVariable,
        poPrintDefaultFormat,
        poEol,
        poDpi,
        poWrap,
        poColumns,
        poTableOfContents,
        poTOCDepth,
        poStripComments,
        poNoHighlight,
        poHighlightStyle,
        poSyntaxDefinition,
        poIncludeInHeader,
        poIncludeBeforeBody,
        poIncludeAfterBody,
        poResourcePath,
        poRequestHeader,
        poSelfContained,
        poHTMLQTags,
        poAscii,
        poReferenceLinks,
        poReferenceLocation,
        poATXHeaders,
        poTopLevelDivision,
        poNumberSections,
        poNumberOffset,
        poListings,
        poIncremental,
        poSlideLevel,
        poSectionDivs,
        poEmailObfuscation,
        poIdPrefix,
        poTitlePrefix,
        poCss,
        poReferenceDoc,
        poEpubCoverImage,
        poEpubMetadata,
        poEpubEmbedFont,
        poEpubChapterLevel,
        poEpubSubdirectory,
        poPDFEngine,
        poPDFEngineOpt,
        poBibliography,
        poCsl,
        poCitationAbbreviations,
        poNatbib,
        poBiblatex,
        poMathjax,
        poMathml,
        poWebtex,
        poKatex,
        poKatexStylesheet,
        poDumpArgs,
        poIgnoreArgs
    );
    TCommandLineOptionMap = specialize TDictionary<TPandocOption, TCommandLineOption>;

    TPandocConverter = class
    private
        class var FPandocPath: String;
        FOptions: TCommandLineOptionMap;
        function CreatePandocProcess(InputPaths: TStringList): TProcess; overload;
        function CreatePandocProcess(): TProcess; overload;
        function HandlePandocProcess(PandocProcess: TProcess): String;
        procedure SetReadFrom(value: String);
        function GetReadFrom(): String;
        procedure SetWriteTo(value: String);
        function GetWriteTo(): String;
        procedure SetOutput(value: String);
        function GetOutput(): String;
        procedure SetDataDir(value: String);
        function GetDataDir(): String;
        procedure SetBashCompletion(value: Boolean);
        function GetBashCompletion(): Boolean;
        procedure SetVerbose(value: Boolean);
        function GetVerbose(): Boolean;
        procedure SetQuiet(value: Boolean);
        function GetQuiet(): Boolean;
        procedure SetFailIfWarnings(value: Boolean);
        function GetFailIfWarnings(): Boolean;
        procedure SetLog(value: String);
        function GetLog(): String;
        procedure SetListInputFormats(value: Boolean);
        function GetListInputFormats(): Boolean;
        procedure SetListOutputFormats(value: Boolean);
        function GetListOutputFormats(): Boolean;
        procedure SetListExtensions(value: Boolean);
        function GetListExtensions(): Boolean;
        procedure SetListHighlightLanguages(value: Boolean);
        function GetListHighlightLanguages(): Boolean;
        procedure SetListHighlightStyles(value: Boolean);
        function GetListHighlightStyles(): Boolean;
        procedure SetVersion(value: Boolean);
        function GetVersion(): Boolean;
        procedure SetHelp(value: Boolean);
        function GetHelp(): Boolean;
        procedure SetBaseHeaderLevel(value: Integer);
        function GetBaseHeaderLevel(): Integer;
        procedure SetIndentedCodeClasses(value: String);
        function GetIndentedCodeClasses(): String;
        procedure SetDefaultImageExtension(value: String);
        function GetDefaultImageExtension(): String;
        procedure SetFileScope(value: Boolean);
        function GetFileScope(): Boolean;
        procedure SetFilter(value: String);
        function GetFilter(): TStringList;
        procedure SetLuaFilter(value: String);
        function GetLuaFilter(): TStringList;
        procedure SetMetadata(value: String);
        function GetMetadata(): TStringList;
        procedure SetPreserveTabs(value: Boolean);
        function GetPreserveTabs(): Boolean;
        procedure SetTabStop(value: Integer);
        function GetTabStop(): Integer;
        procedure SetTrackChanges(value: TTrackChanges);
        function GetTrackChanges(): TTrackChanges;
        procedure SetExtractMedia(value: String);
        function GetExtractMedia(): String;
        procedure SetAbbreviations(value: String);
        function GetAbbreviations(): String;
        procedure SetStandalone(value: Boolean);
        function GetStandalone(): Boolean;
        procedure SetTemplate(value: String);
        function GetTemplate(): String;
        procedure SetVariable(value: String);
        function GetVariable(): TStringList;
        procedure SetPrintDefaultFormat(value: Boolean);
        function GetPrintDefaultFormat(): Boolean;
        procedure SetEol(value: TEol);
        function GetEol(): TEol;
        procedure SetDpi(value: Integer);
        function GetDpi(): Integer;
        procedure SetWrap(value: TWrap);
        function GetWrap(): TWrap;
        procedure SetColumns(value: Integer);
        function GetColumns(): Integer;
        procedure SetTableOfContents(value: Boolean);
        function GetTableOfContents(): Boolean;
        procedure SetTOCDepth(value: Integer);
        function GetTOCDepth(): Integer;
        procedure SetStripComments(value: Boolean);
        function GetStripComments(): Boolean;
        procedure SetNoHighlight(value: Boolean);
        function GetNoHighlight(): Boolean;
        procedure SetHighlightStyle(value: String);
        function GetHighlightStyle(): String;
        procedure SetSyntaxDefinition(value: String);
        function GetSyntaxDefinition(): String;
        procedure SetIncludeInHeader(value: String);
        function GetIncludeInHeader(): TStringList;
        procedure SetIncludeBeforeBody(value: String);
        function GetIncludeBeforeBody(): TStringList;
        procedure SetIncludeAfterBody(value: String);
        function GetIncludeAfterBody(): TStringList;
        procedure SetResourcePath(value: String);
        function GetResourcePath(): String;
        procedure SetRequestHeader(value: String);
        function GetRequestHeader(): TStringList;
        procedure SetSelfContained(value: Boolean);
        function GetSelfContained(): Boolean;
        procedure SetHTMLQTags(value: Boolean);
        function GetHTMLQTags(): Boolean;
        procedure SetAscii(value: Boolean);
        function GetAscii(): Boolean;
        procedure SetReferenceLinks(value: Boolean);
        function GetReferenceLinks(): Boolean;
        procedure SetReferenceLocation(value: TReferenceLocation);
        function GetReferenceLocation(): TReferenceLocation;
        procedure SetATXHeaders(value: Boolean);
        function GetATXHeaders(): Boolean;
        procedure SetTopLevelDivision(value: TTopLevelDivision);
        function GetTopLevelDivision(): TTopLevelDivision;
        procedure SetNumberSections(value: Boolean);
        function GetNumberSections(): Boolean;
        procedure SetNumberOffset(value: String);
        function GetNumberOffset(): String;
        procedure SetListings(value: Boolean);
        function GetListings(): Boolean;
        procedure SetIncremental(value: Boolean);
        function GetIncremental(): Boolean;
        procedure SetSlideLevel(value: Integer);
        function GetSlideLevel(): Integer;
        procedure SetSectionDivs(value: Boolean);
        function GetSectionDivs(): Boolean;
        procedure SetEmailObfuscation(value: TEmailObfuscation);
        function GetEmailObfuscation(): TEmailObfuscation;
        procedure SetIdPrefix(value: String);
        function GetIdPrefix(): String;
        procedure SetTitlePrefix(value: String);
        function GetTitlePrefix(): String;
        procedure SetCss(value: String);
        function GetCss(): TStringList;
        procedure SetReferenceDoc(value: String);
        function GetReferenceDoc(): String;
        procedure SetEpubCoverImage(value: String);
        function GetEpubCoverImage(): String;
        procedure SetEpubMetadata(value: String);
        function GetEpubMetadata(): String;
        procedure SetEpubEmbedFont(value: String);
        function GetEpubEmbedFont(): String;
        procedure SetEpubChapterLevel(value: Integer);
        function GetEpubChapterLevel(): Integer;
        procedure SetEpubSubdirectory(value: String);
        function GetEpubSubdirectory(): String;
        procedure SetPDFEngine(value: TPDFEngine);
        function GetPDFEngine(): TPDFEngine;
        procedure SetPDFEngineOpt(value: String);
        function GetPDFEngineOpt(): TStringList;
        procedure SetBibliography(value: String);
        function GetBibliography(): String;
        procedure SetCsl(value: String);
        function GetCsl(): String;
        procedure SetCitationAbbreviations(value: String);
        function GetCitationAbbreviations(): String;
        procedure SetNatbib(value: Boolean);
        function GetNatbib(): Boolean;
        procedure SetBiblatex(value: Boolean);
        function GetBiblatex(): Boolean;
        procedure SetMathjax(value: String);
        function GetMathjax(): String;
        procedure SetMathml(value: Boolean);
        function GetMathml(): Boolean;
        procedure SetWebtex(value: String);
        function GetWebtex(): String;
        procedure SetKatex(value: String);
        function GetKatex(): String;
        procedure SetKatexStylesheet(value: String);
        function GetKatexStylesheet(): String;
        procedure SetDumpArgs(value: Boolean);
        function GetDumpArgs(): Boolean;
        procedure SetIgnoreArgs(value: Boolean);
        function GetIgnoreArgs(): Boolean;
    public
        property ReadFrom: String read GetReadFrom write SetReadFrom;
        property WriteTo: String read GetWriteTo write SetWriteTo;
        property Output: String read GetOutput write SetOutput;
        property DataDir: String read GetDataDir write SetDataDir;
        property BashCompletion: Boolean read GetBashCompletion write SetBashCompletion;
        property Verbose: Boolean read GetVerbose write SetVerbose;
        property Quiet: Boolean read GetQuiet write SetQuiet;
        property FailIfWarnings: Boolean read GetFailIfWarnings write SetFailIfWarnings;
        property Log: String read GetLog write SetLog;
        property ListInputFormats: Boolean read GetListInputFormats write SetListInputFormats;
        property ListOutputFormats: Boolean read GetListOutputFormats write SetListOutputFormats;
        property ListExtensions: Boolean read GetListExtensions write SetListExtensions;
        property ListHighlightLanguages: Boolean read GetListHighlightLanguages write SetListHighlightLanguages;
        property ListHighlightStyles: Boolean read GetListHighlightStyles write SetListHighlightStyles;
        property Version: Boolean read GetVersion write SetVersion;
        property Help: Boolean read GetHelp write SetHelp;
        property BaseHeaderLevel: Integer read GetBaseHeaderLevel write SetBaseHeaderLevel;
        property IndentedCodeClasses: String read GetIndentedCodeClasses write SetIndentedCodeClasses;
        property DefaultImageExtension: String read GetDefaultImageExtension write SetDefaultImageExtension;
        property FileScope: Boolean read GetFileScope write SetFileScope;
        property FilterList: TStringList read GetFilter;
        property Filter: String write SetFilter;
        property LuaFilterList: TStringList read GetLuaFilter;
        property LuaFilter: String write SetLuaFilter;
        property MetadataList: TStringList read GetMetadata;
        property Metadata: String write SetMetadata;
        property PreserveTabs: Boolean read GetPreserveTabs write SetPreserveTabs;
        property TabStop: Integer read GetTabStop write SetTabStop;
        property TrackChanges: TTrackChanges read GetTrackChanges write SetTrackChanges;
        property ExtractMedia: String read GetExtractMedia write SetExtractMedia;
        property Abbreviations: String read GetAbbreviations write SetAbbreviations;
        property Standalone: Boolean read GetStandalone write SetStandalone;
        property Template: String read GetTemplate write SetTemplate;
        property VariableList: TStringList read GetVariable;
        property Variable: String write SetVariable;
        property PrintDefaultFormat: Boolean read GetPrintDefaultFormat write SetPrintDefaultFormat;
        property Eol: TEol read GetEol write SetEol;
        property Dpi: Integer read GetDpi write SetDpi;
        property Wrap: TWrap read GetWrap write SetWrap;
        property Columns: Integer read GetColumns write SetColumns;
        property TableOfContents: Boolean read GetTableOfContents write SetTableOfContents;
        property TOCDepth: Integer read GetTOCDepth write SetTOCDepth;
        property StripComments: Boolean read GetStripComments write SetStripComments;
        property NoHighlight: Boolean read GetNoHighlight write SetNoHighlight;
        property HighlightStyle: String read GetHighlightStyle write SetHighlightStyle;
        property SyntaxDefinition: String read GetSyntaxDefinition write SetSyntaxDefinition;
        property IncludeInHeaderList: TStringList read GetIncludeInHeader;
        property IncludeInHeader: String write SetIncludeInHeader;
        property IncludeBeforeBodyList: TStringList read GetIncludeBeforeBody;
        property IncludeBeforeBody: String write SetIncludeBeforeBody;
        property IncludeAfterBodyList: TStringList read GetIncludeAfterBody;
        property IncludeAfterBody: String write SetIncludeAfterBody;
        property ResourcePath: String read GetResourcePath write SetResourcePath;
        property RequestHeaderList: TStringList read GetRequestHeader;
        property RequestHeader: String write SetRequestHeader;
        property SelfContained: Boolean read GetSelfContained write SetSelfContained;
        property HTMLQTags: Boolean read GetHTMLQTags write SetHTMLQTags;
        property Ascii: Boolean read GetAscii write SetAscii;
        property ReferenceLinks: Boolean read GetReferenceLinks write SetReferenceLinks;
        property ReferenceLocation: TReferenceLocation read GetReferenceLocation write SetReferenceLocation;
        property ATXHeaders: Boolean read GetATXHeaders write SetATXHeaders;
        property TopLevelDivision: TTopLevelDivision read GetTopLevelDivision write SetTopLevelDivision;
        property NumberSections: Boolean read GetNumberSections write SetNumberSections;
        property NumberOffset: String read GetNumberOffset write SetNumberOffset;
        property Listings: Boolean read GetListings write SetListings;
        property Incremental: Boolean read GetIncremental write SetIncremental;
        property SlideLevel: Integer read GetSlideLevel write SetSlideLevel;
        property SectionDivs: Boolean read GetSectionDivs write SetSectionDivs;
        property EmailObfuscation: TEmailObfuscation read GetEmailObfuscation write SetEmailObfuscation;
        property IdPrefix: String read GetIdPrefix write SetIdPrefix;
        property TitlePrefix: String read GetTitlePrefix write SetTitlePrefix;
        property CssList: TStringList read GetCss;
        property Css: String write SetCss;
        property ReferenceDoc: String read GetReferenceDoc write SetReferenceDoc;
        property EpubCoverImage: String read GetEpubCoverImage write SetEpubCoverImage;
        property EpubMetadata: String read GetEpubMetadata write SetEpubMetadata;
        property EpubEmbedFont: String read GetEpubEmbedFont write SetEpubEmbedFont;
        property EpubChapterLevel: Integer read GetEpubChapterLevel write SetEpubChapterLevel;
        property EpubSubdirectory: String read GetEpubSubdirectory write SetEpubSubdirectory;
        property PDFEngine: TPDFEngine read GetPDFEngine write SetPDFEngine;
        property PDFEngineOptList: TStringList read GetPDFEngineOpt;
        property PDFEngineOpt: String write SetPDFEngineOpt;
        property Bibliography: String read GetBibliography write SetBibliography;
        property Csl: String read GetCsl write SetCsl;
        property CitationAbbreviations: String read GetCitationAbbreviations write SetCitationAbbreviations;
        property Natbib: Boolean read GetNatbib write SetNatbib;
        property Biblatex: Boolean read GetBiblatex write SetBiblatex;
        property Mathjax: String read GetMathjax write SetMathjax;
        property Mathml: Boolean read GetMathml write SetMathml;
        property Webtex: String read GetWebtex write SetWebtex;
        property Katex: String read GetKatex write SetKatex;
        property KatexStylesheet: String read GetKatexStylesheet write SetKatexStylesheet;
        property DumpArgs: Boolean read GetDumpArgs write SetDumpArgs;
        property IgnoreArgs: Boolean read GetIgnoreArgs write SetIgnoreArgs;
        constructor Create();
        class property PandocPath: String read FPandocPath write FPandocPath;
        function HasOption(const Option: TPandocOption): Boolean;
        function ToCommand(): String;
        function Convert(const StringToConvert: String): String;
        function ConvertFile(const PathToFileToConvert: String): String;
        function ConvertFiles(const PathsToFilesToConvert: TStringList): String;
    end;

implementation

    uses 
        SysUtils
    ;

    { ***** TCommandLineOption ***** }
    
    constructor TCommandLineOption.Create(OptionName: String);
    begin
        FName := OptionName;
    end;

    procedure TCommandLineOption.ToParameters(PandocProcess: TProcess; ValueString: String);
    begin
        PandocProcess.Parameters.Add('--' + FName);
        if ValueString <> '' then
            PandocProcess.Parameters.Add(ValueString);
    end;

    { ***** TBooleanCommandLineOption ***** }

    constructor TBooleanCommandLineOption.Create(OptionName: String; OptionValue:
        Boolean);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;

    procedure TBooleanCommandLineOption.ToParameters(PandocProcess: TProcess);
    begin
        if FValue then ToParameters(PandocProcess, '');
    end;

    { ***** TIntegerCommandLineOption ***** }

    constructor TIntegerCommandLineOption.Create(OptionName: String; OptionValue:
        Integer);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;
    
    procedure TIntegerCommandLineOption.ToParameters(PandocProcess: TProcess);
    begin
        ToParameters(PandocProcess, IntToStr(FValue));
    end;

    { ***** TStringCommandLineOption ***** }

    constructor TStringCommandLineOption.Create(OptionName: String; OptionValue:
        String);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;
    
    procedure TStringCommandLineOption.ToParameters(PandocProcess: TProcess);
    begin
        ToParameters(PandocProcess, FValue);
    end;

    { ***** TMultipleCommandLineOption ***** }

    constructor TMultipleCommandLineOption.Create(OptionName: String);
    begin
        inherited Create(OptionName);
        FValue := TStringList.create();
    end;
    
    constructor TMultipleCommandLineOption.Create(OptionName: String;
        OptionValue: TStringList);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;

    destructor TMultipleCommandLineOption.Destroy();
    begin
        FreeAndNil(FValue);
        inherited;
    end;

    procedure TMultipleCommandLineOption.Push(OptionValue: String);
    var
        FoundIndex: Integer;
    begin
        FoundIndex := FValue.IndexOf(OptionValue);
        if -1 < FoundIndex then FValue.Delete(FoundIndex);
        
        FValue.Add(OptionValue);
    end;

    procedure TMultipleCommandLineOption.Remove(OptionValue: String);
    var
        FoundIndex: Integer;
    begin
        FoundIndex := FValue.IndexOf(OptionValue);
        if -1 < FoundIndex then
            FValue.Delete(FoundIndex);
    end;

    procedure TMultipleCommandLineOption.Clear();
    begin
        FValue.Clear();
    end;

    procedure TMultipleCommandLineOption.ToParameters(PandocProcess: TProcess);
    var
        OptionValue: String;
    begin
        for OptionValue in FValue do
            ToParameters(PandocProcess, OptionValue);
    end;



    constructor TTrackChangesCommandLineOption.Create(OptionName: String; OptionValue: TTrackChanges);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;

    procedure TTrackChangesCommandLineOption.ToParameters(PandocProcess: TProcess);
    begin
        case FValue of
            trAccept: ToParameters(PandocProcess, 'accept');
            trReject: ToParameters(PandocProcess, 'reject');
            trAll: ToParameters(PandocProcess, 'all');
        end;
    end;

    constructor TEolCommandLineOption.Create(OptionName: String; OptionValue: TEol);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;

    procedure TEolCommandLineOption.ToParameters(PandocProcess: TProcess);
    begin
        case FValue of
            eoCrlf: ToParameters(PandocProcess, 'crlf');
            eoLf: ToParameters(PandocProcess, 'lf');
            eoNative: ToParameters(PandocProcess, 'native');
        end;
    end;

    constructor TWrapCommandLineOption.Create(OptionName: String; OptionValue: TWrap);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;

    procedure TWrapCommandLineOption.ToParameters(PandocProcess: TProcess);
    begin
        case FValue of
            wrAuto: ToParameters(PandocProcess, 'auto');
            wrNone: ToParameters(PandocProcess, 'none');
            wrPreserve: ToParameters(PandocProcess, 'preserve');
        end;
    end;

    constructor TReferenceLocationCommandLineOption.Create(OptionName: String; OptionValue: TReferenceLocation);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;

    procedure TReferenceLocationCommandLineOption.ToParameters(PandocProcess: TProcess);
    begin
        case FValue of
            reBlock: ToParameters(PandocProcess, 'block');
            reSection: ToParameters(PandocProcess, 'section');
            reDocument: ToParameters(PandocProcess, 'document');
        end;
    end;

    constructor TTopLevelDivisionCommandLineOption.Create(OptionName: String; OptionValue: TTopLevelDivision);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;

    procedure TTopLevelDivisionCommandLineOption.ToParameters(PandocProcess: TProcess);
    begin
        case FValue of
            toDefault: ToParameters(PandocProcess, 'default');
            toSection: ToParameters(PandocProcess, 'section');
            toChapter: ToParameters(PandocProcess, 'chapter');
            toPart: ToParameters(PandocProcess, 'part');
        end;
    end;

    constructor TEmailObfuscationCommandLineOption.Create(OptionName: String; OptionValue: TEmailObfuscation);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;

    procedure TEmailObfuscationCommandLineOption.ToParameters(PandocProcess: TProcess);
    begin
        case FValue of
            emNone: ToParameters(PandocProcess, 'none');
            emJavascript: ToParameters(PandocProcess, 'javascript');
            emReferences: ToParameters(PandocProcess, 'references');
        end;
    end;

    constructor TPDFEngineCommandLineOption.Create(OptionName: String; OptionValue: TPDFEngine);
    begin
        inherited Create(OptionName);
        FValue := OptionValue;
    end;

    procedure TPDFEngineCommandLineOption.ToParameters(PandocProcess: TProcess);
    begin
        case FValue of
            pdPdflatex: ToParameters(PandocProcess, 'pdflatex');
            pdLualatex: ToParameters(PandocProcess, 'lualatex');
            pdXelatex: ToParameters(PandocProcess, 'xelatex');
            pdWkhtmltopdf: ToParameters(PandocProcess, 'wkhtmltopdf');
            pdWeasyprint: ToParameters(PandocProcess, 'weasyprint');
            pdPrince: ToParameters(PandocProcess, 'prince');
            pdContext: ToParameters(PandocProcess, 'context');
            pdPdfroff: ToParameters(PandocProcess, 'pdfroff');
        end;
    end;

    procedure TPandocConverter.SetReadFrom(value: String);
    begin
        if FOptions.ContainsKey(poReadFrom) then
            TStringCommandLineOption(FOptions[poReadFrom]).Value := value
        else
            FOptions.Add(poReadFrom, TStringCommandLineOption.Create('from', value));
    end;

    function TPandocConverter.GetReadFrom(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poReadFrom]).Value;
    end;

    procedure TPandocConverter.SetWriteTo(value: String);
    begin
        if FOptions.ContainsKey(poWriteTo) then
            TStringCommandLineOption(FOptions[poWriteTo]).Value := value
        else
            FOptions.Add(poWriteTo, TStringCommandLineOption.Create('to', value));
    end;

    function TPandocConverter.GetWriteTo(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poWriteTo]).Value;
    end;

    procedure TPandocConverter.SetOutput(value: String);
    begin
        if FOptions.ContainsKey(poOutput) then
            TStringCommandLineOption(FOptions[poOutput]).Value := value
        else
            FOptions.Add(poOutput, TStringCommandLineOption.Create('output', value));
    end;

    function TPandocConverter.GetOutput(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poOutput]).Value;
    end;

    procedure TPandocConverter.SetDataDir(value: String);
    begin
        if FOptions.ContainsKey(poDataDir) then
            TStringCommandLineOption(FOptions[poDataDir]).Value := value
        else
            FOptions.Add(poDataDir, TStringCommandLineOption.Create('data-dir', value));
    end;

    function TPandocConverter.GetDataDir(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poDataDir]).Value;
    end;

    procedure TPandocConverter.SetBashCompletion(value: Boolean);
    begin
        if FOptions.ContainsKey(poBashCompletion) then
            TBooleanCommandLineOption(FOptions[poBashCompletion]).Value := value
        else
            FOptions.Add(poBashCompletion, TBooleanCommandLineOption.Create('bash-completion', value));
    end;

    function TPandocConverter.GetBashCompletion(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poBashCompletion]).Value;
    end;

    procedure TPandocConverter.SetVerbose(value: Boolean);
    begin
        if FOptions.ContainsKey(poVerbose) then
            TBooleanCommandLineOption(FOptions[poVerbose]).Value := value
        else
            FOptions.Add(poVerbose, TBooleanCommandLineOption.Create('verbose', value));
    end;

    function TPandocConverter.GetVerbose(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poVerbose]).Value;
    end;

    procedure TPandocConverter.SetQuiet(value: Boolean);
    begin
        if FOptions.ContainsKey(poQuiet) then
            TBooleanCommandLineOption(FOptions[poQuiet]).Value := value
        else
            FOptions.Add(poQuiet, TBooleanCommandLineOption.Create('quiet', value));
    end;

    function TPandocConverter.GetQuiet(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poQuiet]).Value;
    end;

    procedure TPandocConverter.SetFailIfWarnings(value: Boolean);
    begin
        if FOptions.ContainsKey(poFailIfWarnings) then
            TBooleanCommandLineOption(FOptions[poFailIfWarnings]).Value := value
        else
            FOptions.Add(poFailIfWarnings, TBooleanCommandLineOption.Create('fail-if-warnings', value));
    end;

    function TPandocConverter.GetFailIfWarnings(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poFailIfWarnings]).Value;
    end;

    procedure TPandocConverter.SetLog(value: String);
    begin
        if FOptions.ContainsKey(poLog) then
            TStringCommandLineOption(FOptions[poLog]).Value := value
        else
            FOptions.Add(poLog, TStringCommandLineOption.Create('log', value));
    end;

    function TPandocConverter.GetLog(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poLog]).Value;
    end;

    procedure TPandocConverter.SetListInputFormats(value: Boolean);
    begin
        if FOptions.ContainsKey(poListInputFormats) then
            TBooleanCommandLineOption(FOptions[poListInputFormats]).Value := value
        else
            FOptions.Add(poListInputFormats, TBooleanCommandLineOption.Create('list-input-formats', value));
    end;

    function TPandocConverter.GetListInputFormats(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poListInputFormats]).Value;
    end;

    procedure TPandocConverter.SetListOutputFormats(value: Boolean);
    begin
        if FOptions.ContainsKey(poListOutputFormats) then
            TBooleanCommandLineOption(FOptions[poListOutputFormats]).Value := value
        else
            FOptions.Add(poListOutputFormats, TBooleanCommandLineOption.Create('list-output-formats', value));
    end;

    function TPandocConverter.GetListOutputFormats(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poListOutputFormats]).Value;
    end;

    procedure TPandocConverter.SetListExtensions(value: Boolean);
    begin
        if FOptions.ContainsKey(poListExtensions) then
            TBooleanCommandLineOption(FOptions[poListExtensions]).Value := value
        else
            FOptions.Add(poListExtensions, TBooleanCommandLineOption.Create('list-extensions', value));
    end;

    function TPandocConverter.GetListExtensions(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poListExtensions]).Value;
    end;

    procedure TPandocConverter.SetListHighlightLanguages(value: Boolean);
    begin
        if FOptions.ContainsKey(poListHighlightLanguages) then
            TBooleanCommandLineOption(FOptions[poListHighlightLanguages]).Value := value
        else
            FOptions.Add(poListHighlightLanguages, TBooleanCommandLineOption.Create('list-highlight-languages', value));
    end;

    function TPandocConverter.GetListHighlightLanguages(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poListHighlightLanguages]).Value;
    end;

    procedure TPandocConverter.SetListHighlightStyles(value: Boolean);
    begin
        if FOptions.ContainsKey(poListHighlightStyles) then
            TBooleanCommandLineOption(FOptions[poListHighlightStyles]).Value := value
        else
            FOptions.Add(poListHighlightStyles, TBooleanCommandLineOption.Create('list-highlight-styles', value));
    end;

    function TPandocConverter.GetListHighlightStyles(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poListHighlightStyles]).Value;
    end;

    procedure TPandocConverter.SetVersion(value: Boolean);
    begin
        if FOptions.ContainsKey(poVersion) then
            TBooleanCommandLineOption(FOptions[poVersion]).Value := value
        else
            FOptions.Add(poVersion, TBooleanCommandLineOption.Create('version', value));
    end;

    function TPandocConverter.GetVersion(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poVersion]).Value;
    end;

    procedure TPandocConverter.SetHelp(value: Boolean);
    begin
        if FOptions.ContainsKey(poHelp) then
            TBooleanCommandLineOption(FOptions[poHelp]).Value := value
        else
            FOptions.Add(poHelp, TBooleanCommandLineOption.Create('help', value));
    end;

    function TPandocConverter.GetHelp(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poHelp]).Value;
    end;

    procedure TPandocConverter.SetBaseHeaderLevel(value: Integer);
    begin
        if FOptions.ContainsKey(poBaseHeaderLevel) then
            TIntegerCommandLineOption(FOptions[poBaseHeaderLevel]).Value := value
        else
            FOptions.Add(poBaseHeaderLevel, TIntegerCommandLineOption.Create('base-header-level', value));
    end;

    function TPandocConverter.GetBaseHeaderLevel(): Integer;
    begin
        Result := TIntegerCommandLineOption(FOptions[poBaseHeaderLevel]).Value;
    end;

    procedure TPandocConverter.SetIndentedCodeClasses(value: String);
    begin
        if FOptions.ContainsKey(poIndentedCodeClasses) then
            TStringCommandLineOption(FOptions[poIndentedCodeClasses]).Value := value
        else
            FOptions.Add(poIndentedCodeClasses, TStringCommandLineOption.Create('indented-code-classes', value));
    end;

    function TPandocConverter.GetIndentedCodeClasses(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poIndentedCodeClasses]).Value;
    end;

    procedure TPandocConverter.SetDefaultImageExtension(value: String);
    begin
        if FOptions.ContainsKey(poDefaultImageExtension) then
            TStringCommandLineOption(FOptions[poDefaultImageExtension]).Value := value
        else
            FOptions.Add(poDefaultImageExtension, TStringCommandLineOption.Create('default-image-extension', value));
    end;

    function TPandocConverter.GetDefaultImageExtension(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poDefaultImageExtension]).Value;
    end;

    procedure TPandocConverter.SetFileScope(value: Boolean);
    begin
        if FOptions.ContainsKey(poFileScope) then
            TBooleanCommandLineOption(FOptions[poFileScope]).Value := value
        else
            FOptions.Add(poFileScope, TBooleanCommandLineOption.Create('file-scope', value));
    end;

    function TPandocConverter.GetFileScope(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poFileScope]).Value;
    end;

    procedure TPandocConverter.SetFilter(value: String);
    begin
        if not FOptions.ContainsKey(poFilter) then
            FOptions.Add(poFilter, TMultipleCommandLineOption.Create('filter'));
       TMultipleCommandLineOption(FOptions[poFilter]).Push(value);
    end;

    function TPandocConverter.GetFilter(): TStringList;
    begin
        Result := TMultipleCommandLineOption(FOptions[poFilter]).Value;
    end;

    procedure TPandocConverter.SetLuaFilter(value: String);
    begin
        if not FOptions.ContainsKey(poLuaFilter) then
            FOptions.Add(poLuaFilter, TMultipleCommandLineOption.Create('lua-filter'));
       TMultipleCommandLineOption(FOptions[poLuaFilter]).Push(value);
    end;

    function TPandocConverter.GetLuaFilter(): TStringList;
    begin
        Result := TMultipleCommandLineOption(FOptions[poLuaFilter]).Value;
    end;

    procedure TPandocConverter.SetMetadata(value: String);
    begin
        if not FOptions.ContainsKey(poMetadata) then
            FOptions.Add(poMetadata, TMultipleCommandLineOption.Create('metadata'));
       TMultipleCommandLineOption(FOptions[poMetadata]).Push(value);
    end;

    function TPandocConverter.GetMetadata(): TStringList;
    begin
        Result := TMultipleCommandLineOption(FOptions[poMetadata]).Value;
    end;

    procedure TPandocConverter.SetPreserveTabs(value: Boolean);
    begin
        if FOptions.ContainsKey(poPreserveTabs) then
            TBooleanCommandLineOption(FOptions[poPreserveTabs]).Value := value
        else
            FOptions.Add(poPreserveTabs, TBooleanCommandLineOption.Create('preserve-tabs', value));
    end;

    function TPandocConverter.GetPreserveTabs(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poPreserveTabs]).Value;
    end;

    procedure TPandocConverter.SetTabStop(value: Integer);
    begin
        if FOptions.ContainsKey(poTabStop) then
            TIntegerCommandLineOption(FOptions[poTabStop]).Value := value
        else
            FOptions.Add(poTabStop, TIntegerCommandLineOption.Create('tab-stop', value));
    end;

    function TPandocConverter.GetTabStop(): Integer;
    begin
        Result := TIntegerCommandLineOption(FOptions[poTabStop]).Value;
    end;

    procedure TPandocConverter.SetTrackChanges(value: TTrackChanges);
    begin
        if FOptions.ContainsKey(poTrackChanges) then
            TTrackChangesCommandLineOption(FOptions[poTrackChanges]).Value := value
        else
            FOptions.Add(poTrackChanges, TTrackChangesCommandLineOption.Create('track-changes', value));
    end;

    function TPandocConverter.GetTrackChanges(): TTrackChanges;
    begin
        Result := TTrackChangesCommandLineOption(FOptions[poTrackChanges]).Value;
    end;

    procedure TPandocConverter.SetExtractMedia(value: String);
    begin
        if FOptions.ContainsKey(poExtractMedia) then
            TStringCommandLineOption(FOptions[poExtractMedia]).Value := value
        else
            FOptions.Add(poExtractMedia, TStringCommandLineOption.Create('extract-media', value));
    end;

    function TPandocConverter.GetExtractMedia(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poExtractMedia]).Value;
    end;

    procedure TPandocConverter.SetAbbreviations(value: String);
    begin
        if FOptions.ContainsKey(poAbbreviations) then
            TStringCommandLineOption(FOptions[poAbbreviations]).Value := value
        else
            FOptions.Add(poAbbreviations, TStringCommandLineOption.Create('abbreviations', value));
    end;

    function TPandocConverter.GetAbbreviations(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poAbbreviations]).Value;
    end;

    procedure TPandocConverter.SetStandalone(value: Boolean);
    begin
        if FOptions.ContainsKey(poStandalone) then
            TBooleanCommandLineOption(FOptions[poStandalone]).Value := value
        else
            FOptions.Add(poStandalone, TBooleanCommandLineOption.Create('standalone', value));
    end;

    function TPandocConverter.GetStandalone(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poStandalone]).Value;
    end;

    procedure TPandocConverter.SetTemplate(value: String);
    begin
        if FOptions.ContainsKey(poTemplate) then
            TStringCommandLineOption(FOptions[poTemplate]).Value := value
        else
            FOptions.Add(poTemplate, TStringCommandLineOption.Create('template', value));
    end;

    function TPandocConverter.GetTemplate(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poTemplate]).Value;
    end;

    procedure TPandocConverter.SetVariable(value: String);
    begin
        if not FOptions.ContainsKey(poVariable) then
            FOptions.Add(poVariable, TMultipleCommandLineOption.Create('variable'));
       TMultipleCommandLineOption(FOptions[poVariable]).Push(value);
    end;

    function TPandocConverter.GetVariable(): TStringList;
    begin
        Result := TMultipleCommandLineOption(FOptions[poVariable]).Value;
    end;

    procedure TPandocConverter.SetPrintDefaultFormat(value: Boolean);
    begin
        if FOptions.ContainsKey(poPrintDefaultFormat) then
            TBooleanCommandLineOption(FOptions[poPrintDefaultFormat]).Value := value
        else
            FOptions.Add(poPrintDefaultFormat, TBooleanCommandLineOption.Create('print-default-format', value));
    end;

    function TPandocConverter.GetPrintDefaultFormat(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poPrintDefaultFormat]).Value;
    end;

    procedure TPandocConverter.SetEol(value: TEol);
    begin
        if FOptions.ContainsKey(poEol) then
            TEolCommandLineOption(FOptions[poEol]).Value := value
        else
            FOptions.Add(poEol, TEolCommandLineOption.Create('eol', value));
    end;

    function TPandocConverter.GetEol(): TEol;
    begin
        Result := TEolCommandLineOption(FOptions[poEol]).Value;
    end;

    procedure TPandocConverter.SetDpi(value: Integer);
    begin
        if FOptions.ContainsKey(poDpi) then
            TIntegerCommandLineOption(FOptions[poDpi]).Value := value
        else
            FOptions.Add(poDpi, TIntegerCommandLineOption.Create('dpi', value));
    end;

    function TPandocConverter.GetDpi(): Integer;
    begin
        Result := TIntegerCommandLineOption(FOptions[poDpi]).Value;
    end;

    procedure TPandocConverter.SetWrap(value: TWrap);
    begin
        if FOptions.ContainsKey(poWrap) then
            TWrapCommandLineOption(FOptions[poWrap]).Value := value
        else
            FOptions.Add(poWrap, TWrapCommandLineOption.Create('wrap', value));
    end;

    function TPandocConverter.GetWrap(): TWrap;
    begin
        Result := TWrapCommandLineOption(FOptions[poWrap]).Value;
    end;

    procedure TPandocConverter.SetColumns(value: Integer);
    begin
        if FOptions.ContainsKey(poColumns) then
            TIntegerCommandLineOption(FOptions[poColumns]).Value := value
        else
            FOptions.Add(poColumns, TIntegerCommandLineOption.Create('columns', value));
    end;

    function TPandocConverter.GetColumns(): Integer;
    begin
        Result := TIntegerCommandLineOption(FOptions[poColumns]).Value;
    end;

    procedure TPandocConverter.SetTableOfContents(value: Boolean);
    begin
        if FOptions.ContainsKey(poTableOfContents) then
            TBooleanCommandLineOption(FOptions[poTableOfContents]).Value := value
        else
            FOptions.Add(poTableOfContents, TBooleanCommandLineOption.Create('table-of-contents', value));
    end;

    function TPandocConverter.GetTableOfContents(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poTableOfContents]).Value;
    end;

    procedure TPandocConverter.SetTOCDepth(value: Integer);
    begin
        if FOptions.ContainsKey(poTOCDepth) then
            TIntegerCommandLineOption(FOptions[poTOCDepth]).Value := value
        else
            FOptions.Add(poTOCDepth, TIntegerCommandLineOption.Create('toc-depth', value));
    end;

    function TPandocConverter.GetTOCDepth(): Integer;
    begin
        Result := TIntegerCommandLineOption(FOptions[poTOCDepth]).Value;
    end;

    procedure TPandocConverter.SetStripComments(value: Boolean);
    begin
        if FOptions.ContainsKey(poStripComments) then
            TBooleanCommandLineOption(FOptions[poStripComments]).Value := value
        else
            FOptions.Add(poStripComments, TBooleanCommandLineOption.Create('strip-comments', value));
    end;

    function TPandocConverter.GetStripComments(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poStripComments]).Value;
    end;

    procedure TPandocConverter.SetNoHighlight(value: Boolean);
    begin
        if FOptions.ContainsKey(poNoHighlight) then
            TBooleanCommandLineOption(FOptions[poNoHighlight]).Value := value
        else
            FOptions.Add(poNoHighlight, TBooleanCommandLineOption.Create('no-highlight', value));
    end;

    function TPandocConverter.GetNoHighlight(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poNoHighlight]).Value;
    end;

    procedure TPandocConverter.SetHighlightStyle(value: String);
    begin
        if FOptions.ContainsKey(poHighlightStyle) then
            TStringCommandLineOption(FOptions[poHighlightStyle]).Value := value
        else
            FOptions.Add(poHighlightStyle, TStringCommandLineOption.Create('highlight-style', value));
    end;

    function TPandocConverter.GetHighlightStyle(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poHighlightStyle]).Value;
    end;

    procedure TPandocConverter.SetSyntaxDefinition(value: String);
    begin
        if FOptions.ContainsKey(poSyntaxDefinition) then
            TStringCommandLineOption(FOptions[poSyntaxDefinition]).Value := value
        else
            FOptions.Add(poSyntaxDefinition, TStringCommandLineOption.Create('syntax-definition', value));
    end;

    function TPandocConverter.GetSyntaxDefinition(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poSyntaxDefinition]).Value;
    end;

    procedure TPandocConverter.SetIncludeInHeader(value: String);
    begin
        if not FOptions.ContainsKey(poIncludeInHeader) then
            FOptions.Add(poIncludeInHeader, TMultipleCommandLineOption.Create('include-in-header'));
       TMultipleCommandLineOption(FOptions[poIncludeInHeader]).Push(value);
    end;

    function TPandocConverter.GetIncludeInHeader(): TStringList;
    begin
        Result := TMultipleCommandLineOption(FOptions[poIncludeInHeader]).Value;
    end;

    procedure TPandocConverter.SetIncludeBeforeBody(value: String);
    begin
        if not FOptions.ContainsKey(poIncludeBeforeBody) then
            FOptions.Add(poIncludeBeforeBody, TMultipleCommandLineOption.Create('include-before-body'));
       TMultipleCommandLineOption(FOptions[poIncludeBeforeBody]).Push(value);
    end;

    function TPandocConverter.GetIncludeBeforeBody(): TStringList;
    begin
        Result := TMultipleCommandLineOption(FOptions[poIncludeBeforeBody]).Value;
    end;

    procedure TPandocConverter.SetIncludeAfterBody(value: String);
    begin
        if not FOptions.ContainsKey(poIncludeAfterBody) then
            FOptions.Add(poIncludeAfterBody, TMultipleCommandLineOption.Create('include-after-body'));
       TMultipleCommandLineOption(FOptions[poIncludeAfterBody]).Push(value);
    end;

    function TPandocConverter.GetIncludeAfterBody(): TStringList;
    begin
        Result := TMultipleCommandLineOption(FOptions[poIncludeAfterBody]).Value;
    end;

    procedure TPandocConverter.SetResourcePath(value: String);
    begin
        if FOptions.ContainsKey(poResourcePath) then
            TStringCommandLineOption(FOptions[poResourcePath]).Value := value
        else
            FOptions.Add(poResourcePath, TStringCommandLineOption.Create('resource-path', value));
    end;

    function TPandocConverter.GetResourcePath(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poResourcePath]).Value;
    end;

    procedure TPandocConverter.SetRequestHeader(value: String);
    begin
        if not FOptions.ContainsKey(poRequestHeader) then
            FOptions.Add(poRequestHeader, TMultipleCommandLineOption.Create('request-header'));
       TMultipleCommandLineOption(FOptions[poRequestHeader]).Push(value);
    end;

    function TPandocConverter.GetRequestHeader(): TStringList;
    begin
        Result := TMultipleCommandLineOption(FOptions[poRequestHeader]).Value;
    end;

    procedure TPandocConverter.SetSelfContained(value: Boolean);
    begin
        if FOptions.ContainsKey(poSelfContained) then
            TBooleanCommandLineOption(FOptions[poSelfContained]).Value := value
        else
            FOptions.Add(poSelfContained, TBooleanCommandLineOption.Create('self-contained', value));
    end;

    function TPandocConverter.GetSelfContained(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poSelfContained]).Value;
    end;

    procedure TPandocConverter.SetHTMLQTags(value: Boolean);
    begin
        if FOptions.ContainsKey(poHTMLQTags) then
            TBooleanCommandLineOption(FOptions[poHTMLQTags]).Value := value
        else
            FOptions.Add(poHTMLQTags, TBooleanCommandLineOption.Create('html-q-tags', value));
    end;

    function TPandocConverter.GetHTMLQTags(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poHTMLQTags]).Value;
    end;

    procedure TPandocConverter.SetAscii(value: Boolean);
    begin
        if FOptions.ContainsKey(poAscii) then
            TBooleanCommandLineOption(FOptions[poAscii]).Value := value
        else
            FOptions.Add(poAscii, TBooleanCommandLineOption.Create('ascii', value));
    end;

    function TPandocConverter.GetAscii(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poAscii]).Value;
    end;

    procedure TPandocConverter.SetReferenceLinks(value: Boolean);
    begin
        if FOptions.ContainsKey(poReferenceLinks) then
            TBooleanCommandLineOption(FOptions[poReferenceLinks]).Value := value
        else
            FOptions.Add(poReferenceLinks, TBooleanCommandLineOption.Create('reference-links', value));
    end;

    function TPandocConverter.GetReferenceLinks(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poReferenceLinks]).Value;
    end;

    procedure TPandocConverter.SetReferenceLocation(value: TReferenceLocation);
    begin
        if FOptions.ContainsKey(poReferenceLocation) then
            TReferenceLocationCommandLineOption(FOptions[poReferenceLocation]).Value := value
        else
            FOptions.Add(poReferenceLocation, TReferenceLocationCommandLineOption.Create('reference-location', value));
    end;

    function TPandocConverter.GetReferenceLocation(): TReferenceLocation;
    begin
        Result := TReferenceLocationCommandLineOption(FOptions[poReferenceLocation]).Value;
    end;

    procedure TPandocConverter.SetATXHeaders(value: Boolean);
    begin
        if FOptions.ContainsKey(poATXHeaders) then
            TBooleanCommandLineOption(FOptions[poATXHeaders]).Value := value
        else
            FOptions.Add(poATXHeaders, TBooleanCommandLineOption.Create('atx-headers', value));
    end;

    function TPandocConverter.GetATXHeaders(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poATXHeaders]).Value;
    end;

    procedure TPandocConverter.SetTopLevelDivision(value: TTopLevelDivision);
    begin
        if FOptions.ContainsKey(poTopLevelDivision) then
            TTopLevelDivisionCommandLineOption(FOptions[poTopLevelDivision]).Value := value
        else
            FOptions.Add(poTopLevelDivision, TTopLevelDivisionCommandLineOption.Create('top-level-division', value));
    end;

    function TPandocConverter.GetTopLevelDivision(): TTopLevelDivision;
    begin
        Result := TTopLevelDivisionCommandLineOption(FOptions[poTopLevelDivision]).Value;
    end;

    procedure TPandocConverter.SetNumberSections(value: Boolean);
    begin
        if FOptions.ContainsKey(poNumberSections) then
            TBooleanCommandLineOption(FOptions[poNumberSections]).Value := value
        else
            FOptions.Add(poNumberSections, TBooleanCommandLineOption.Create('number-sections', value));
    end;

    function TPandocConverter.GetNumberSections(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poNumberSections]).Value;
    end;

    procedure TPandocConverter.SetNumberOffset(value: String);
    begin
        if FOptions.ContainsKey(poNumberOffset) then
            TStringCommandLineOption(FOptions[poNumberOffset]).Value := value
        else
            FOptions.Add(poNumberOffset, TStringCommandLineOption.Create('number-offset', value));
    end;

    function TPandocConverter.GetNumberOffset(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poNumberOffset]).Value;
    end;

    procedure TPandocConverter.SetListings(value: Boolean);
    begin
        if FOptions.ContainsKey(poListings) then
            TBooleanCommandLineOption(FOptions[poListings]).Value := value
        else
            FOptions.Add(poListings, TBooleanCommandLineOption.Create('listings', value));
    end;

    function TPandocConverter.GetListings(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poListings]).Value;
    end;

    procedure TPandocConverter.SetIncremental(value: Boolean);
    begin
        if FOptions.ContainsKey(poIncremental) then
            TBooleanCommandLineOption(FOptions[poIncremental]).Value := value
        else
            FOptions.Add(poIncremental, TBooleanCommandLineOption.Create('incremental', value));
    end;

    function TPandocConverter.GetIncremental(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poIncremental]).Value;
    end;

    procedure TPandocConverter.SetSlideLevel(value: Integer);
    begin
        if FOptions.ContainsKey(poSlideLevel) then
            TIntegerCommandLineOption(FOptions[poSlideLevel]).Value := value
        else
            FOptions.Add(poSlideLevel, TIntegerCommandLineOption.Create('slide-level', value));
    end;

    function TPandocConverter.GetSlideLevel(): Integer;
    begin
        Result := TIntegerCommandLineOption(FOptions[poSlideLevel]).Value;
    end;

    procedure TPandocConverter.SetSectionDivs(value: Boolean);
    begin
        if FOptions.ContainsKey(poSectionDivs) then
            TBooleanCommandLineOption(FOptions[poSectionDivs]).Value := value
        else
            FOptions.Add(poSectionDivs, TBooleanCommandLineOption.Create('section-divs', value));
    end;

    function TPandocConverter.GetSectionDivs(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poSectionDivs]).Value;
    end;

    procedure TPandocConverter.SetEmailObfuscation(value: TEmailObfuscation);
    begin
        if FOptions.ContainsKey(poEmailObfuscation) then
            TEmailObfuscationCommandLineOption(FOptions[poEmailObfuscation]).Value := value
        else
            FOptions.Add(poEmailObfuscation, TEmailObfuscationCommandLineOption.Create('email-obfuscation', value));
    end;

    function TPandocConverter.GetEmailObfuscation(): TEmailObfuscation;
    begin
        Result := TEmailObfuscationCommandLineOption(FOptions[poEmailObfuscation]).Value;
    end;

    procedure TPandocConverter.SetIdPrefix(value: String);
    begin
        if FOptions.ContainsKey(poIdPrefix) then
            TStringCommandLineOption(FOptions[poIdPrefix]).Value := value
        else
            FOptions.Add(poIdPrefix, TStringCommandLineOption.Create('id-prefix', value));
    end;

    function TPandocConverter.GetIdPrefix(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poIdPrefix]).Value;
    end;

    procedure TPandocConverter.SetTitlePrefix(value: String);
    begin
        if FOptions.ContainsKey(poTitlePrefix) then
            TStringCommandLineOption(FOptions[poTitlePrefix]).Value := value
        else
            FOptions.Add(poTitlePrefix, TStringCommandLineOption.Create('title-prefix', value));
    end;

    function TPandocConverter.GetTitlePrefix(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poTitlePrefix]).Value;
    end;

    procedure TPandocConverter.SetCss(value: String);
    begin
        if not FOptions.ContainsKey(poCss) then
            FOptions.Add(poCss, TMultipleCommandLineOption.Create('css'));
       TMultipleCommandLineOption(FOptions[poCss]).Push(value);
    end;

    function TPandocConverter.GetCss(): TStringList;
    begin
        Result := TMultipleCommandLineOption(FOptions[poCss]).Value;
    end;

    procedure TPandocConverter.SetReferenceDoc(value: String);
    begin
        if FOptions.ContainsKey(poReferenceDoc) then
            TStringCommandLineOption(FOptions[poReferenceDoc]).Value := value
        else
            FOptions.Add(poReferenceDoc, TStringCommandLineOption.Create('reference-doc', value));
    end;

    function TPandocConverter.GetReferenceDoc(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poReferenceDoc]).Value;
    end;

    procedure TPandocConverter.SetEpubCoverImage(value: String);
    begin
        if FOptions.ContainsKey(poEpubCoverImage) then
            TStringCommandLineOption(FOptions[poEpubCoverImage]).Value := value
        else
            FOptions.Add(poEpubCoverImage, TStringCommandLineOption.Create('epub-cover-image', value));
    end;

    function TPandocConverter.GetEpubCoverImage(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poEpubCoverImage]).Value;
    end;

    procedure TPandocConverter.SetEpubMetadata(value: String);
    begin
        if FOptions.ContainsKey(poEpubMetadata) then
            TStringCommandLineOption(FOptions[poEpubMetadata]).Value := value
        else
            FOptions.Add(poEpubMetadata, TStringCommandLineOption.Create('epub-metadata', value));
    end;

    function TPandocConverter.GetEpubMetadata(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poEpubMetadata]).Value;
    end;

    procedure TPandocConverter.SetEpubEmbedFont(value: String);
    begin
        if FOptions.ContainsKey(poEpubEmbedFont) then
            TStringCommandLineOption(FOptions[poEpubEmbedFont]).Value := value
        else
            FOptions.Add(poEpubEmbedFont, TStringCommandLineOption.Create('epub-embed-font', value));
    end;

    function TPandocConverter.GetEpubEmbedFont(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poEpubEmbedFont]).Value;
    end;

    procedure TPandocConverter.SetEpubChapterLevel(value: Integer);
    begin
        if FOptions.ContainsKey(poEpubChapterLevel) then
            TIntegerCommandLineOption(FOptions[poEpubChapterLevel]).Value := value
        else
            FOptions.Add(poEpubChapterLevel, TIntegerCommandLineOption.Create('epub-chapter-level', value));
    end;

    function TPandocConverter.GetEpubChapterLevel(): Integer;
    begin
        Result := TIntegerCommandLineOption(FOptions[poEpubChapterLevel]).Value;
    end;

    procedure TPandocConverter.SetEpubSubdirectory(value: String);
    begin
        if FOptions.ContainsKey(poEpubSubdirectory) then
            TStringCommandLineOption(FOptions[poEpubSubdirectory]).Value := value
        else
            FOptions.Add(poEpubSubdirectory, TStringCommandLineOption.Create('epub-subdirectory', value));
    end;

    function TPandocConverter.GetEpubSubdirectory(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poEpubSubdirectory]).Value;
    end;

    procedure TPandocConverter.SetPDFEngine(value: TPDFEngine);
    begin
        if FOptions.ContainsKey(poPDFEngine) then
            TPDFEngineCommandLineOption(FOptions[poPDFEngine]).Value := value
        else
            FOptions.Add(poPDFEngine, TPDFEngineCommandLineOption.Create('pdf-engine', value));
    end;

    function TPandocConverter.GetPDFEngine(): TPDFEngine;
    begin
        Result := TPDFEngineCommandLineOption(FOptions[poPDFEngine]).Value;
    end;

    procedure TPandocConverter.SetPDFEngineOpt(value: String);
    begin
        if not FOptions.ContainsKey(poPDFEngineOpt) then
            FOptions.Add(poPDFEngineOpt, TMultipleCommandLineOption.Create('pdf-engine-opt'));
       TMultipleCommandLineOption(FOptions[poPDFEngineOpt]).Push(value);
    end;

    function TPandocConverter.GetPDFEngineOpt(): TStringList;
    begin
        Result := TMultipleCommandLineOption(FOptions[poPDFEngineOpt]).Value;
    end;

    procedure TPandocConverter.SetBibliography(value: String);
    begin
        if FOptions.ContainsKey(poBibliography) then
            TStringCommandLineOption(FOptions[poBibliography]).Value := value
        else
            FOptions.Add(poBibliography, TStringCommandLineOption.Create('bibliography', value));
    end;

    function TPandocConverter.GetBibliography(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poBibliography]).Value;
    end;

    procedure TPandocConverter.SetCsl(value: String);
    begin
        if FOptions.ContainsKey(poCsl) then
            TStringCommandLineOption(FOptions[poCsl]).Value := value
        else
            FOptions.Add(poCsl, TStringCommandLineOption.Create('csl', value));
    end;

    function TPandocConverter.GetCsl(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poCsl]).Value;
    end;

    procedure TPandocConverter.SetCitationAbbreviations(value: String);
    begin
        if FOptions.ContainsKey(poCitationAbbreviations) then
            TStringCommandLineOption(FOptions[poCitationAbbreviations]).Value := value
        else
            FOptions.Add(poCitationAbbreviations, TStringCommandLineOption.Create('citation-abbreviations', value));
    end;

    function TPandocConverter.GetCitationAbbreviations(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poCitationAbbreviations]).Value;
    end;

    procedure TPandocConverter.SetNatbib(value: Boolean);
    begin
        if FOptions.ContainsKey(poNatbib) then
            TBooleanCommandLineOption(FOptions[poNatbib]).Value := value
        else
            FOptions.Add(poNatbib, TBooleanCommandLineOption.Create('natbib', value));
    end;

    function TPandocConverter.GetNatbib(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poNatbib]).Value;
    end;

    procedure TPandocConverter.SetBiblatex(value: Boolean);
    begin
        if FOptions.ContainsKey(poBiblatex) then
            TBooleanCommandLineOption(FOptions[poBiblatex]).Value := value
        else
            FOptions.Add(poBiblatex, TBooleanCommandLineOption.Create('biblatex', value));
    end;

    function TPandocConverter.GetBiblatex(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poBiblatex]).Value;
    end;

    procedure TPandocConverter.SetMathjax(value: String);
    begin
        if FOptions.ContainsKey(poMathjax) then
            TStringCommandLineOption(FOptions[poMathjax]).Value := value
        else
            FOptions.Add(poMathjax, TStringCommandLineOption.Create('mathjax', value));
    end;

    function TPandocConverter.GetMathjax(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poMathjax]).Value;
    end;

    procedure TPandocConverter.SetMathml(value: Boolean);
    begin
        if FOptions.ContainsKey(poMathml) then
            TBooleanCommandLineOption(FOptions[poMathml]).Value := value
        else
            FOptions.Add(poMathml, TBooleanCommandLineOption.Create('mathml', value));
    end;

    function TPandocConverter.GetMathml(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poMathml]).Value;
    end;

    procedure TPandocConverter.SetWebtex(value: String);
    begin
        if FOptions.ContainsKey(poWebtex) then
            TStringCommandLineOption(FOptions[poWebtex]).Value := value
        else
            FOptions.Add(poWebtex, TStringCommandLineOption.Create('webtex', value));
    end;

    function TPandocConverter.GetWebtex(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poWebtex]).Value;
    end;

    procedure TPandocConverter.SetKatex(value: String);
    begin
        if FOptions.ContainsKey(poKatex) then
            TStringCommandLineOption(FOptions[poKatex]).Value := value
        else
            FOptions.Add(poKatex, TStringCommandLineOption.Create('katex', value));
    end;

    function TPandocConverter.GetKatex(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poKatex]).Value;
    end;

    procedure TPandocConverter.SetKatexStylesheet(value: String);
    begin
        if FOptions.ContainsKey(poKatexStylesheet) then
            TStringCommandLineOption(FOptions[poKatexStylesheet]).Value := value
        else
            FOptions.Add(poKatexStylesheet, TStringCommandLineOption.Create('katex-stylesheet', value));
    end;

    function TPandocConverter.GetKatexStylesheet(): String;
    begin
        Result := TStringCommandLineOption(FOptions[poKatexStylesheet]).Value;
    end;

    procedure TPandocConverter.SetDumpArgs(value: Boolean);
    begin
        if FOptions.ContainsKey(poDumpArgs) then
            TBooleanCommandLineOption(FOptions[poDumpArgs]).Value := value
        else
            FOptions.Add(poDumpArgs, TBooleanCommandLineOption.Create('dump-args', value));
    end;

    function TPandocConverter.GetDumpArgs(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poDumpArgs]).Value;
    end;

    procedure TPandocConverter.SetIgnoreArgs(value: Boolean);
    begin
        if FOptions.ContainsKey(poIgnoreArgs) then
            TBooleanCommandLineOption(FOptions[poIgnoreArgs]).Value := value
        else
            FOptions.Add(poIgnoreArgs, TBooleanCommandLineOption.Create('ignore-args', value));
    end;

    function TPandocConverter.GetIgnoreArgs(): Boolean;
    begin
        Result := TBooleanCommandLineOption(FOptions[poIgnoreArgs]).Value;
    end;


    function DeterminePandocPath(): String;
    var 
        Path: String;
    begin
        if TPandocConverter.PandocPath <> '' then
            Result := TPandocConverter.PandocPath
        else
        begin
            Path := GetEnvironmentVariable('PAS_PANDOC_PATH');
            if Path <> '' then
                Result := Path
            else   
                Result := 'pandoc';
        end
    end;

    constructor TPandocConverter.Create();
    begin
        FOptions := TCommandLineOptionMap.Create;
        PandocPath := DeterminePandocPath;
    end;

    function TPandocConverter.HasOption(const Option: TPandocOption): Boolean;
    begin
        Result := FOptions.ContainsKey(Option);
    end;

    function TPandocConverter.CreatePandocProcess(InputPaths: TStringList): TProcess;
    var
        Option: TCommandLineOption;
        InputPath: String;
    begin
        Result := TProcess.Create(nil);
        Result.Options := [poUsePipes];

        Result.Executable := PandocPath;

        for Option in FOptions.Values do
            Option.ToParameters(Result);

        for InputPath in InputPaths do
            if InputPath <> '' then
                Result.Parameters.Add(InputPath);
    end;

    function TPandocConverter.CreatePandocProcess(): TProcess;
    begin
        Result := CreatePandocProcess(TStringList.Create);
    end;

    function TPandocConverter.ToCommand(): String;
    var
        PandocProcess: TProcess;
        Option: String;
    begin
        PandocProcess := CreatePandocProcess;
        Result := PandocProcess.Executable;
        for Option in PandocProcess.Parameters do
        begin
            Result := Result + ' ' + Option;
        end
    end;

    function TPandocConverter.HandlePandocProcess(PandocProcess: TProcess): String;
    begin
        PandocProcess.CloseInput;
    
        {Read from STDOUT}
        with TStringList.Create do
        begin
            LoadFromStream(PandocProcess.Output);
            Result := Text;
        end;

        {Read from STDERR}
        with TStringList.Create do
        begin
            LoadFromStream(PandocProcess.Stderr);
            WriteLn(Text);
        end;
        PandocProcess.Free;
    end;

    function TPandocConverter.Convert(const StringToConvert: String): String;
    var
        PandocProcess: TProcess;
    begin
        PandocProcess := CreatePandocProcess;
        PandocProcess.Execute;
        PandocProcess.Input.Write(Pointer(StringToConvert + LineEnding)^, Length(StringToConvert) + 1);
        Result := HandlePandocProcess(PandocProcess);
    end;

    function TPandocConverter.ConvertFiles(const PathsToFilesToConvert: TStringList): String;
    var
        PandocProcess: TProcess;
    begin
        PandocProcess := CreatePandocProcess(PathsToFilesToConvert);
        PandocProcess.Execute;
        Result := HandlePandocProcess(PandocProcess);
    end;

    function TPandocConverter.ConvertFile(const PathToFileToConvert: String): String;
    var
        InputPaths: TStringList;
    begin
        InputPaths := TStringList.Create;
        InputPaths.Add(PathToFileToConvert);
        Result := ConvertFiles(InputPaths);
    end;

end.
