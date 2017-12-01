        constructor Create();
        class property PandocPath: String read FPandocPath write FPandocPath;
        function HasOption(const Option: TPandocOption): Boolean;
        function ToCommand(): String;
        function Convert(const StringToConvert: String): String;
        function ConvertFile(const PathToFileToConvert: String): String;
        function ConvertFiles(const PathsToFilesToConvert: TStringList): String;
    end;
