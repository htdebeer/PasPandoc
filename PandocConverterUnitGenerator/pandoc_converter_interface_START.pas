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

