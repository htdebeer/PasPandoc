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


