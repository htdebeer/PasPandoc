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

    Generates the @link(PandocConverter) unit in the src subdirectory

}
program GeneratePasPandocConverterModule;
{$mode objfpc}
uses
    classes,
    SysUtils,
    PasPandocConverterUnitGenerator
    ;

var
    PasPandocUnitGenerator: TPasPandocUnitGenerator;
begin
    PasPandocUnitGenerator := TPasPandocUnitGenerator.FromConfiguration;
    PasPandocUnitGenerator.GenerateUnit('../src/PandocConverter.pas');
end.
