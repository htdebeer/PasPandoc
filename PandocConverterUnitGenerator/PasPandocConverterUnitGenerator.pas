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
unit PasPandocConverterUnitGenerator;
{$mode objfpc}
{$H+}
{$J-}

interface

uses
    Classes,
    Generics.Collections
    ;

const
    PANDOC_OPTIONS_JSON_FILENAME = 'pandoc_options.json';
    PANDOC_CONVERTER_UNIT_PATH = 'PandocConverter.pas';

    INTERFACE_START_FILE = 'pandoc_converter_interface_START.pas';
    INTERFACE_END_FILE = 'pandoc_converter_interface_END.pas';
    
    IMPLEMENTATION_START_FILE = 'pandoc_converter_implementation_START.pas';
    IMPLEMENTATION_END_FILE = 'pandoc_converter_implementation_END.pas';

type

    TOption = class
    private
        FName: String;
        FType: String;
        FUseName: String;
        FMultiple: Boolean;

        function GetPOName(): String;
        function GetOptionType(): String; virtual;
        function GetCommandLineOptionType(): String;
        function GetGetter(): String;
        function GetSetter(): String;
        function GetPropertyName(): String;
        function GetIsMultiple(): Boolean;
    public
        constructor Create(OptionName: String; UseOptionName: String; OptionType: String = 'String'); overload;

        function GenerateTypeDeclaration(): String; virtual;
        function GenerateOptionTypeDeclaration(): String; virtual;
        function GenerateOptionTypeImplementation(): String; virtual;
        function GenerateSetterDeclaration(): String;
        function GenerateSetterImplementation(): String;
        function GenerateGetterDeclaration(): String;
        function GenerateGetterImplementation(): String;
        function GeneratePropertyDeclaration(): String;
        
        property Name: String read FName;
        property UseName: String read FUseName write FUseName;
        property POName: String read GetPOName;
        property OptionType: String read GetOptionType;
        property CommandLineOptionType: String read GetCommandLineOptionType;
        property Getter: String read GetGetter;
        property Setter: String read GetSetter;
        property PropertyName: String read GetPropertyName;
        property IsMultiple: Boolean read GetIsMultiple write FMultiple;
    end;
    
    TEnumOption = class(TOption)
    private
        FValues: TStringList;
        function GetOptionType(): String; override;
    public
        constructor Create(OptionName: String); overload;
        constructor Create(OptionName: String; UseOptionName: String); overload;
        constructor Create(OptionName: String; EnumValues: TStringList); overload;
        constructor Create(OptionName: String; UseOptionName: String; EnumValues: TStringList); overload;

        destructor Destroy(); override;

        function GenerateTypeDeclaration(): String; override;
        function GenerateOptionTypeDeclaration(): String; override;
        function GenerateOptionTypeImplementation(): String; override;
        
        property Values: TStringList read FValues write FValues;
    end;

    TOptionList = specialize TList<TOption>;

    TPasPandocUnitGenerator = class
    private
        FOptions: TOptionList;
        function GeneratePandocOptionType(): String;
        function GenerateInterface(): String;
        function GenerateImplementation(): String;
        constructor Create(Options: TOptionList);
    public
        destructor Destroy; override;
        class function FromConfiguration(Path: String = PANDOC_OPTIONS_JSON_FILENAME): TPasPandocUnitGenerator;
        procedure GenerateUnit(Path: String = PANDOC_CONVERTER_UNIT_PATH);
    end;

implementation

uses 
    SysUtils,
    fpjson, 
    jsonparser
    ;

function Capitalize(str: String): String;
begin
    if str = '' then
        Result := ''
    else
        Result := UpperCase(Copy(str, 1, 1)) + Copy(str, 2, Length(str) - 1);
end;

function FileToString(FileName: String): String;
var
    str: TStringList;
begin
    str := TStringList.Create;
    str.LoadFromFile(FileName);
    Result := str.GetText;
end;

constructor TOption.Create(OptionName: String; UseOptionName: String; OptionType: String = 'String');
begin
    FName := OptionName;
    FUseName := UseOptionName;
    FType := OptionType;
end;

function TOption.GetPOName(): String;
begin
    Result := 'po' + Capitalize(UseName);
end;

function TOption.GetOptionType(): String;
begin
    Result := Capitalize(FType);
end;

function TOption.GetIsMultiple(): Boolean;
begin
    Result := FMultiple and (LowerCase(FType) = 'string');
end;

function TOption.GetCommandLineOptionType(): String;
begin
    if IsMultiple then
        Result := 'TMultipleCommandLineOption'
    else
        Result := 'T' + Capitalize(FType) + 'CommandLineOption';
end;

function TOption.GetPropertyName(): String;
begin
    Result := Capitalize(UseName);
end;

function TOption.GetGetter(): String;
begin
    Result := 'Get' + PropertyName;
end;

function TOption.GetSetter(): String;
begin
    Result := 'Set' + PropertyName;
end;

function TOption.GenerateTypeDeclaration(): String;
begin
    Result := '';
end;
        
function TOption.GenerateOptionTypeDeclaration(): String;
begin
    Result := '';
end;
        
function TOption.GenerateOptionTypeImplementation(): String;
begin
    Result := '';
end;

function TOption.GenerateSetterDeclaration(): String;
begin
    Result :=
        '        procedure ' + Setter + '(value: ' + OptionType + ');';
end;

function TOption.GenerateSetterImplementation(): String;
begin
    if IsMultiple then
        Result := 
            '    procedure TPandocConverter.' + Setter + '(value: ' + OptionType + ');' + LineEnding +
            '    begin' + LineEnding +
            '        if not FOptions.ContainsKey(' + POName + ') then' + LineEnding +
            '            FOptions.Add(' + POName + ', ' + CommandLineOptionType + '.Create(''' + FName + '''));' + LineEnding + 
            '       ' + CommandLineOptionType + '(FOptions[' + POName + ']).Push(value);' + LineEnding +
            '    end;'
    else 
        Result := 
            '    procedure TPandocConverter.' + Setter + '(value: ' + OptionType + ');' + LineEnding +
            '    begin' + LineEnding +
            '        if FOptions.ContainsKey(' + POName + ') then' + LineEnding +
            '            ' + CommandLineOptionType + '(FOptions[' + POName + ']).Value := value' + LineEnding +
            '        else' + LineEnding +
            '            FOptions.Add(' + POName + ', ' + CommandLineOptionType + '.Create(''' + FName + ''', value));' + LineEnding + 
            '    end;';
end;

function TOption.GenerateGetterDeclaration(): String;
var
    OptionTypeToUse: String;
begin
    if IsMultiple then
        OptionTypeToUse := 'TStringList'
    else
        OptionTypeToUse := OptionType;
    
    Result := 
        '        function ' + Getter + '(): ' + OptionTypeToUse + ';';
end;

function TOption.GenerateGetterImplementation(): String;
var
    OptionTypeToUse: String;
begin
    if IsMultiple then
        OptionTypeToUse := 'TStringList'
    else
        OptionTypeToUse := OptionType;

    Result := 
        '    function TPandocConverter.' + Getter + '(): ' + OptionTypeToUse + ';' + LineEnding +
        '    begin' + LineEnding +
        '        Result := ' + CommandLineOptionType + '(FOptions[' + POName + ']).Value;' + LineEnding +
        '    end;';
end;

function TOption.GeneratePropertyDeclaration(): String;
begin
    if IsMultiple then
    begin
        Result := 
            '        property ' + PropertyName + 'List: TStringList read ' + Getter + ';' + LineEnding;
        Result := Result + 
            '        property ' + PropertyName + ': ' + OptionType + ' write ' + Setter + ';'
    end
    else
        Result := 
            '        property ' + PropertyName + ': ' + OptionType + ' read ' + Getter + ' write ' + Setter + ';';
end;

constructor TEnumOption.Create(OptionName: String);
begin
    Create(OptionName, OptionName);
end;

constructor TEnumOption.Create(OptionName: String; UseOptionName: String);
begin
    Create(OptionName, UseOptionName, TStringList.Create);
end;

constructor TEnumOption.Create(OptionName: String; EnumValues: TStringList);
begin
    Create(OptionName, OptionName, EnumValues);
end;

constructor TEnumOption.Create(OptionName: String; UseOptionName: String; EnumValues: TStringList);
begin
    inherited Create(OptionName, UseOptionName, Capitalize(UseOptionName));
    FValues := EnumValues;
end;

function TEnumOption.GetOptionType(): String;
begin
    Result := 'T' + Capitalize(UseName);
end;

destructor TEnumOption.Destroy();
begin
    FreeAndNil(FValues);
    inherited;
end;

function TEnumOption.GenerateTypeDeclaration(): String;
var
    Index: Integer;
    EnumValue: String;
    EnumValueName: String;
begin
    Result := '    ' + OptionType + ' = (' + LineEnding;
    Index := 1;
    for EnumValue in FValues do
    begin
        EnumValueName := LowerCase(Copy(FName, 1, 2)) + Capitalize(EnumValue);
        Result := Result + '        ' + EnumValueName;

        if Index < FValues.Count then 
            Result := Result + ',' + LineEnding
        else
            Result := Result + LineEnding;

        Index := Index + 1;
    end;
    Result := Result + '    );'
end;
        
function TEnumOption.GenerateOptionTypeDeclaration(): String;
begin
    Result := '    ' +
        CommandLineOptionType + ' = class(TCommandLineOption)' + LineEnding +
        '    private' + LineEnding +
        '        FValue: ' + OptionType + ';' + LineEnding +
        '    public' + LineEnding +
        '        property Value: ' + OptionType + ' read FValue write FValue;' + LineEnding +
        '        constructor Create(OptionName: String; OptionValue: ' + OptionType + ');' + LineEnding +
        '        procedure ToParameters(PandocProcess: TProcess); override;' + LineEnding +
        '    end;'    
    ;
end;
        
function TEnumOption.GenerateOptionTypeImplementation(): String;
var
    EnumValue: String;
    EnumValueName: String;
begin
    Result :=
        '    constructor ' + CommandLineOptionType + '.Create(OptionName: String; OptionValue: ' + OptionType + ');' + LineEnding +
        '    begin' + LineEnding +
        '        inherited Create(OptionName);' + LineEnding +
        '        FValue := OptionValue;' + LineEnding +
        '    end;' + LineEnding
        ;

    Result := Result + LineEnding;
    Result := Result +
        '    procedure ' + CommandLineOptionType + '.ToParameters(PandocProcess: TProcess);' + LineEnding +
        '    begin' + LineEnding +
        '        case FValue of' + LineEnding
        ;

    for EnumValue in FValues do
    begin
        EnumValueName := LowerCase(Copy(FName, 1, 2)) + Capitalize(EnumValue);
        Result := Result + '            ' + EnumValueName + ': ToParameters(PandocProcess, ''' + EnumValue + ''');' + LineEnding;
    end;

    Result := Result +
        '        end;' + LineEnding +
        '    end;' + LineEnding
        ;
end;

constructor TPasPandocUnitGenerator.Create(Options: TOptionList);
begin
    FOptions := Options;
end;

destructor TPasPandocUnitGenerator.Destroy;
begin
    FreeAndNil(FOptions);
    inherited;
end;

class function TPasPandocUnitGenerator.FromConfiguration(Path: String = PANDOC_OPTIONS_JSON_FILENAME): TPasPandocUnitGenerator;
    function HasOption(OptionToFind: TOption; Options: TOptionList): Boolean;
    var
        Option: TOption;
    begin
        Result := False;
        for Option in Options do
        begin
            if OptionToFind.Name = Option.Name then
                Result := True;
        end;
    end;

    function GetEnumValues(JSONEnumValues: TJSONData): TStringList;
    var
        JSONEnumValue: TJSONEnum;
        EnumValue: String;
    begin
        Result := TStringList.Create;
        for JSONEnumValue in JSONEnumValues do
        begin
            EnumValue := LowerCase(JSONEnumValue.Value.AsString);
            if -1 = Result.IndexOf(EnumValue) then
                Result.Add(EnumValue);
        end;
    end;

    function GetOption(JSONOption: TJSONEnum): TOption;
    const
        OPTION_TYPE = 'type';
        OPTION_USE_NAME = 'use-name';
        OPTION_MULTIPLE = 'multiple';
        OPTION_ENUM_VALUES = 'values';

        TYPE_ENUM = 'enum';
        TYPE_STRING = 'string';
    var
        PandocOptionName: String;
        PandocUseOptionName: String;
        PandocOptionType: String;
    begin
        PandocOptionName := JSONOption.Key;
        
        if nil <> JSONOption.Value.FindPath(OPTION_USE_NAME) then
            PandocUseOptionName := JSONOption.Value.GetPath(OPTION_USE_NAME).AsString
        else
            PandocUseOptionName := PandocOptionName;
        
        PandocOptionType := JSONOption.Value.GetPath(OPTION_TYPE).AsString;

        if TYPE_ENUM = PandocOptionType then
            Result := TEnumOption.Create(
                PandocOptionName, 
                PandocUseOptionName,
                GetEnumValues(JSONOption.Value.GetPath(OPTION_ENUM_VALUES))
            )
        else
            Result := TOption.Create(
                PandocOptionName, 
                PandocUseOptionName, 
                PandocOptionType
            );

        if nil <> JSONOption.Value.FindPath(OPTION_MULTIPLE) then
        begin
            Result.IsMultiple := JSONOption.Value.GetPath(OPTION_MULTIPLE).AsBoolean;
            if TYPE_STRING <> LowerCase(PandocOptionType) then
                WriteLn('Warning for option ''' + PandocOptionName + ''': multiple command line options should have type string');
        end
    end;

var
    JsonInStream: TFileStream;
    JsonOption: TJSONENum;
    PandocOptions: TOptionList;
    Option: TOption;
begin
    PandocOptions := TOptionList.Create();
    JsonInStream := TFileStream.Create(Path, fmOpenRead);

    try
        for JsonOption in GetJSON(JSONInStream) do
        begin
            Option := GetOption(JsonOption);
            if HasOption(Option, PandocOptions) then
                WriteLn('Warning option ''' + Option.Name + ''' already used. An option can occur only once.')
            else
                PandocOptions.Add(Option);
        end;
    finally
        JsonInStream.free;
    end;
    
    Result := TPasPandocUnitGenerator.Create(PandocOptions);
end;

function TPasPandocUnitGenerator.GeneratePandocOptionType(): String;
var
    Index: Integer;
    PandocOption: TOption;
begin
    Index := 1;
    Result := '    TPandocOption = (' + LineEnding;
    for PandocOption in FOptions do
    begin
        Result := Result + '        ' + PandocOption.POName;

        if Index < FOptions.Count then
            Result := Result + ',' + LineEnding
        else
            Result := Result + LineEnding;

        Index := Index + 1;
    end;
    Result := Result + '    );' + LineEnding;
end;

function TPasPandocUnitGenerator.GenerateInterface: String;
var
    PandocOption: TOption;
begin
    Result := FileToString(INTERFACE_START_FILE) + LineEnding;
    for PandocOption in FOptions do
    begin
        if PandocOption is TEnumOption then
        begin
            Result := Result + Pandocoption.GenerateTypeDeclaration() + LineEnding;
            Result := Result + LineEnding;
            Result := Result + PandocOption.GenerateOptionTypeDeclaration() + LineEnding;
            Result := Result + LineEnding;
        end
    end;
    Result := Result + LineEnding;

    Result := Result + GeneratePandocOptionType + LineEnding;

    Result := Result + 
        '    TCommandLineOptionMap = specialize TDictionary<TPandocOption, TCommandLineOption>;' + LineEnding;
    
    Result := Result + LineEnding;
    Result := Result + 
        '    TPandocConverter = class' + LineEnding +
        '    private' + LineEnding +
        '        class var FPandocPath: String;' + LineEnding +  
        '        FOptions: TCommandLineOptionMap;' + LineEnding +
        LineEnding +
        '        function CreatePandocProcess(InputPaths: TStringList): TProcess; overload;' + LineEnding +
        '        function CreatePandocProcess(): TProcess; overload;' + LineEnding +
        '        function HandlePandocProcess(PandocProcess: TProcess): String;' + LineEnding
        ;

    for PandocOption in FOptions do
    begin
        Result := Result + PandocOption.GenerateSetterDeclaration() + LineEnding;
        Result := Result + PandocOption.GenerateGetterDeclaration() + LineEnding;
    end;

    Result := Result + '    public' + LineEnding;

    for PandocOption in FOptions do
    begin
        Result := Result + PandocOption.GeneratePropertyDeclaration() + LineEnding;
    end;

    Result := Result + FileToString(INTERFACE_END_FILE);
end;

function TPasPandocUnitGenerator.GenerateImplementation(): String;
var
    PandocOption: TOption;
begin
    Result := FileToString(IMPLEMENTATION_START_FILE) + LineEnding;
    
    for PandocOption in FOptions do
    begin
        if PandocOption is TEnumOption then
        begin
            Result := Result + PandocOption.GenerateOptionTypeImplementation() + LineEnding;
        end
    end;
    
    for PandocOption in FOptions do
    begin
        Result := Result + PandocOption.GenerateSetterImplementation() + LineEnding;
        Result := Result + LineEnding;
        Result := Result + PandocOption.GenerateGetterImplementation() + LineEnding;
        Result := Result + LineEnding;
    end;
    
    Result := Result + FileToString(IMPLEMENTATION_END_FILE);
end;


procedure TPasPandocUnitGenerator.GenerateUnit(Path: String = PANDOC_CONVERTER_UNIT_PATH);
var
    UnitOutStream: TFileStream;
    StringToWrite: String;
begin
    UnitOutStream := TFileStream.Create(Path, fmCreate);
    try
    try
        StringToWrite := GenerateInterface + LineEnding + GenerateImplementation;
        UnitOutStream.Write(Pointer(StringToWrite)^, Length(StringToWrite));
    except on E: Exception do
                WriteLn('Something went wrong writing file: ' + E.message);
    end;
    finally
        UnitOutStream.free;
    end;
end;

end.
