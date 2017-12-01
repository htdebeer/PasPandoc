
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

    {
        Handle the result of running pandoc. Check both the STDOUT as well as
        STDERR and the exit code to determine success or failure.

        TODO: actual error handling
    }
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
        PandocProcess.Input.Write(Pointer(StringToConvert)^, Length(StringToConvert));
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
