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
unit util;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, constants, fgl, Controls;

type
  TControlList = specialize TFPGList<TControl>;
  { TSequence }

  // ToDo: Make TSequence Thread safe
  TSequence = class(TObject)
  private
    FCounter: longword;
    procedure Increment;
  public
    constructor Create();
    destructor Destroy; override;
    function CurrVal(): longword;
    function NextVal(): longword;
    procedure Reset;
  end;

  { TUserInterfaceMetrics }

  TUserInterfaceMetrics = class(TObject)
  private
    FPadding: integer;
    FMargin: integer;
    FReferenceForm: TForm;
    procedure SetReferenceForm(AValue: TForm);
    procedure Recalculate;
  public
    constructor Create();
    destructor Destroy; override;
    property Padding: integer read FPadding;
    property Margin: integer read FMargin;
    property ReferenceForm: TForm read FReferenceForm write SetReferenceForm;
  end;

var
  UserInterfaceMetrics: TUserInterfaceMetrics = nil;

implementation

{ TUserInterfaceMetrics }

procedure TUserInterfaceMetrics.SetReferenceForm(AValue: TForm);
begin
  if FReferenceForm = AValue then Exit;
  FReferenceForm := AValue;
  Recalculate;
end;

procedure TUserInterfaceMetrics.Recalculate;
var
  SystemFont: TFont;
  SystemFontSize: integer;
begin
  if Assigned(FReferenceForm) then
    SystemFont := FReferenceForm.Font
  else
    SystemFont := Screen.SystemFont;

  SystemFontSize := Abs(Round(
    (GetFontData(SystemFont.Handle).Height * 72 / SystemFont.PixelsPerInch)));

  //TempCanvas := TCanvas.Create;

  //SystemFontSize := TempCanvas.Font.Size;
  //TempCanvas.Free;
  //SystemFontSize:=8;

  {Divide the default pitch by padding ratio an then round to the uppermost 4 }
  FPadding := ((((SystemFontSize div PADDING_RATIO) + PADDING_ROUNDED_TO - 1) shr
    1) shl 1);
  FMargin := ((((SystemFontSize div MARGIN_RATIO) + MARGIN_ROUNDED_TO - 1) shr
    2) shl 2);
end;

constructor TUserInterfaceMetrics.Create;
begin
  FReferenceForm := nil;
  Recalculate;

end;

destructor TUserInterfaceMetrics.Destroy;
begin
  inherited Destroy;
end;

{ TSequence }

procedure TSequence.Increment;
begin
  Inc(FCounter);
end;

constructor TSequence.Create();
begin
  //FCounter := 0;
  Reset;
end;

destructor TSequence.Destroy;
begin
  inherited Destroy;
end;

function TSequence.CurrVal(): longword;
begin
  Result := FCounter;
end;

function TSequence.NextVal(): longword;
begin
  Increment;
  Result := FCounter;
end;

procedure TSequence.Reset;
begin
  FCounter := 0;
end;

initialization
  UserInterfaceMetrics := TUserInterfaceMetrics.Create;

finalization
  FreeAndNil(UserInterfaceMetrics);
end.
