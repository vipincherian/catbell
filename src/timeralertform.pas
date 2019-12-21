{

Copyright (C) 2019 Vipin Cherian

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
unit timeralertform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType, ExtCtrls, Buttons, LazLogger;

type

  { TfrmTimerAlert }

  TfrmTimerAlert = class(TForm)
    bbClose: TBitBtn;
    Label1: TLabel;
    lbMessages: TListBox;
    FTimer: TTimer;
    procedure bbCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { public declarations }
  end;

var
  frmTimerAlert: TfrmTimerAlert;

implementation

{$R *.lfm}

{ TfrmTimerAlert }

procedure TfrmTimerAlert.FormCreate(Sender: TObject);
begin
  //BorderStyle := bsNone;
  //OnShow := @FormShow;
  //AlphaBlend:=True;
  AlphaBlendValue:=50;
end;

procedure TfrmTimerAlert.FormShow(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  //FTimer.Interval:=2000;
end;

procedure TfrmTimerAlert.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_THICKFRAME;
end;

procedure TfrmTimerAlert.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmTimerAlert.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  //DebugLn('Entering FormClose');
  lbMessages.Items.Clear;
  //DebugLn('Exiting FormClose');
end;

end.

