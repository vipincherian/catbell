{

Copyright (C) 2021 Vipin Cherian

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the
Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA  02110-1301, USA.

}
unit sequence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TSequence }

  // ToDo: Make TSequence Thread safe
  TSequence = class(TObject)
  private
    FCounter: LongWord;
    procedure Increment;
  public
    constructor Create();
    Destructor  Destroy; override;
    function  CurrVal(): Longword;
    function NextVal(): Longword;
  end;

implementation

{ TSequence }

procedure TSequence.Increment;
begin
  Inc(FCounter);
end;

constructor TSequence.Create;
begin
  FCounter := 0;
end;

destructor TSequence.Destroy;
begin
  inherited Destroy;
end;

function TSequence.CurrVal: Longword;
begin
     Result := FCounter;
end;

function TSequence.NextVal: Longword;
begin
     Increment;
     Result := FCounter;
end;

end.

