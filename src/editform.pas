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
unit editform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  DateTimePicker, settings, dateutils;

type
  { TTimerSpecs }
  {TTimerSpecs = class(TObject)
  private
  public
    DurationHours: integer;
    DurationMinutes: integer;
    DurationSeconds: integer;
    //ModalAlert: boolean;
    //TrayNotification: boolean;
    Description: string;
    ModalAlert: boolean;
    TrayNotification: boolean;
    constructor Create();
    destructor Destroy; override;
  end;}

  { TfrmEditTimer }

  TfrmEditTimer = class(TForm)
    bbSelectAudioFile: TBitBtn;
    bbCancel: TBitBtn;
    bbSave: TBitBtn;
    ckbModalAlert: TCheckBox;
    ckbTrayNotification: TCheckBox;
    dtpDuration: TDateTimePicker;
    edtDescription: TEdit;
    edtAudioFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure bbCancelClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure dtpDurationChange(Sender: TObject);
    procedure edtDescriptionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FProceed: boolean;
    //FSpecs: TTimerSpecs;
    FDuration: TTime;
    FDescription: string;
    FModalAlert: boolean;
    FTrayNotification: boolean;
    FId: longword;
    procedure SetDescription(AValue: string);
    procedure SetDuration(AValue: TTime);
    procedure SetFTrayNotification(AValue: boolean);
    procedure SetModalAlert(AValue: boolean);
    function Validate: boolean;
  public
    function ShowAndGetSpecs: boolean;
    function ShowForAdd :boolean;
    function ShowForEdit (Sender: TFrame):boolean;
    //property Specs: TTimerSpecs read FSpecs;
    property Duration: TTime read FDuration write SetDuration;
    property Description: string read FDescription write SetDescription;
    property ModalAlert: boolean read FModalAlert write SetModalAlert;
    property TrayNotification: boolean read FTrayNotification write SetFTrayNotification;
    property Id: longword read FId;
  end;


var
  frmEditTimer: TfrmEditTimer;

implementation

uses
  timerframe;

{$R *.lfm}

{ TTimerSpecs }

{procedure TTimerSpecs.SetDurationHours(AValue: integer);
begin
  FDurationHours := AValue;
end;

procedure TTimerSpecs.SetDescription(AValue: string);
begin
  if FDescription='' then Exit;
  FDescription:=AValue;
end;}

{constructor TTimerSpecs.Create();
begin

end;

destructor TTimerSpecs.Destroy;
begin
  inherited Destroy;
end;}

{ TfrmEditTimer }

procedure TfrmEditTimer.FormCreate(Sender: TObject);
begin
  FProceed:=False;

  //FSpecs:=TTimerSpecs.Create();
{  FSpecs.DurationHours:=0;
  FSpecs.DurationMinutes:=10;
  FSpecs.DurationSeconds:=0;
  FSpecs.Description:='Countdown Timer';}
  with GlobalUserConfig do
  begin
    FDuration:=EncodeTime(DefaultTimerHours, DefaultTimerMins, DefaultTimerSecs,0);
    FDescription:=DefaultTimerTitle;
    FModalAlert:=ShowModalAlert;
    FTrayNotification:=ShowTrayAlert;
  end;

  bbSave.Enabled:=Validate;

end;

procedure TfrmEditTimer.bbSaveClick(Sender: TObject);
{var
  Hour, Min, Sec, Milli : Word;}
begin
  FDescription:=edtDescription.Text;
  {DecodeTime(dtpDuration.Time, Hour, Min, Sec, Milli);
  FSpecs.DurationHours:=Hour;
  FSpecs.DurationMinutes:=Min;
  FSpecs.DurationSeconds:=Sec;}
  FDuration:=dtpDuration.Time;
  FModalAlert:=ckbModalAlert.Checked;
  FTrayNotification:=ckbTrayNotification.Checked;
  FProceed:=True;
  Close;
end;

procedure TfrmEditTimer.dtpDurationChange(Sender: TObject);
begin
  bbSave.Enabled:=Validate;
end;

procedure TfrmEditTimer.edtDescriptionChange(Sender: TObject);
begin
  bbSave.Enabled:=Validate;
end;

procedure TfrmEditTimer.bbCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmEditTimer.FormDestroy(Sender: TObject);
begin
  //FSpecs.Free;
end;

procedure TfrmEditTimer.FormShow(Sender: TObject);
begin

end;

function TfrmEditTimer.Validate: boolean;
var
  Hours, Minutes, Seconds: word;
begin
  if edtDescription.Text = '' then
  begin
    Result := False;
    Exit;
  end;

  Hours:= HourOf(dtpDuration.Time);
  Minutes:= MinuteOf(dtpDuration.Time);
  Seconds:= SecondOf(dtpDuration.Time);

  if((Hours = 0) and (Minutes = 0) and (Seconds = 0)) then
  begin
    Result := False;
    Exit;
  end;

  Result:= True;

end;

procedure TfrmEditTimer.SetDescription(AValue: string);
begin
  FDescription:=AValue;
  edtDescription.Text:=AValue;
end;

procedure TfrmEditTimer.SetDuration(AValue: TTime);
begin
  FDuration:=AValue;
  dtpDuration.Time:=AValue;
end;

procedure TfrmEditTimer.SetFTrayNotification(AValue: boolean);
begin
  //if FTrayNotification=AValue then Exit;
  FTrayNotification:=AValue;
  ckbTrayNotification.Checked:=AValue;
end;

procedure TfrmEditTimer.SetModalAlert(AValue: boolean);
begin
  //if FModalAlert=AValue then Exit;
  FModalAlert:=AValue;
  ckbModalAlert.Checked:=AValue;
end;

function TfrmEditTimer.ShowAndGetSpecs: boolean;
begin
  FProceed:=False;
  {edtDescription.Text:=FSpecs.Description;
  dtpDuration.Time := EncodeTime(FSpecs.DurationHours, FSpecs.DurationMinutes,  FSpecs.DurationSeconds, 0);
  ckbModalAlert.Checked:=FSpecs.ModalAlert;
  ckbTrayNotification.Checked:=Specs.TrayNotification; }
  ShowModal;
  Result:=FProceed;
end;

function TfrmEditTimer.ShowForAdd: boolean;
begin
  Self.Caption:='Add Timer';
  FId := longword(-1);
  Result:=ShowAndGetSpecs;
end;

function TfrmEditTimer.ShowForEdit(Sender: TFrame): boolean;
var
  Widget: TfraTimer;
begin
  if Sender = Nil then
    Exit;
  Widget := TfraTimer(Sender);
  Fid := Widget.Id;
  Caption:='Edit Timer';
  //if Widget.Running then
  dtpDuration.Enabled:=(not Widget.Running);
  Result:=ShowAndGetSpecs;
  FId := longword(-1);
end;

end.
