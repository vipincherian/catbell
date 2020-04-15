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
  ComCtrls, DateTimePicker, settings, dateutils, {sndfile, ctypes,} LazLogger,
  Math, audio;

type
  TTimerAudioInfo = record
    FileName: string;
    Duration: double;
    Looped: boolean;
  end;

  { TfrmEdit }

  TfrmEdit = class(TForm)
    bbCancel: TBitBtn;
    bbClearAudioFile: TBitBtn;
    bbSave: TBitBtn;
    bbSelectAudioFile: TBitBtn;
    ckbUseDefaultSound: TCheckBox;
    ckbLoop: TCheckBox;
    ckbModalAlert: TCheckBox;
    ckbTrayNotification: TCheckBox;
    dtpDuration: TDateTimePicker;
    edtAudioFile: TEdit;
    edtDescription: TEdit;
    imlEdit: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblLengthText: TLabel;
    lblLenthVal: TLabel;
    odgAudio: TOpenDialog;
    pgEditTimer: TPageControl;
    tsTimer: TTabSheet;
    tsSound: TTabSheet;
    procedure bbCancelClick(Sender: TObject);
    procedure bbClearAudioFileClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure bbSelectAudioFileClick(Sender: TObject);
    procedure ckbUseDefaultSoundChange(Sender: TObject);
    procedure dtpByChange(Sender: TObject);
    procedure dtpDurationChange(Sender: TObject);
    procedure edtDescriptionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FProceed: boolean;
    FDuration: TTime;
    FDescription: string;
    FModalAlert: boolean;
    FTrayNotification: boolean;
    FUseDefaultSound: boolean;
    FId: longword;

    FAudio: TAudio;
    FAudioInfo: TTimerAudioInfo;
    function GetAudioDuration: double;
    function GetAudioFileName: string;
    function GetAudioLooped: boolean;
    procedure SetAudio(AValue: TAudio);
    procedure SetAudioDuration(AValue: double);
    procedure SetAudioFileName(AValue: string);
    procedure SetAudioLooped(AValue: boolean);
    procedure SetDescription(AValue: string);
    procedure SetDuration(AValue: TTime);
    procedure SetFTrayNotification(AValue: boolean);
    procedure SetModalAlert(AValue: boolean);
    procedure SetUseDefaultSound(AValue: boolean);
    function Validate: boolean;
  public
    function ShowAndGetSpecs: boolean;
    function ShowForAdd: boolean;
    function ShowForEdit(Sender: TFrame): boolean;

    property Duration: TTime read FDuration write SetDuration;
    property Description: string read FDescription write SetDescription;
    property ModalAlert: boolean read FModalAlert write SetModalAlert;

    property TrayNotification: boolean read FTrayNotification write SetFTrayNotification;
    property UseDefaultSound: boolean read FUseDefaultSound write SetUseDefaultSound;
    property Id: longword read FId;

    property Audio: TAudio read FAudio write SetAudio;
    property AudioFileName: string read GetAudioFileName write SetAudioFileName;
    property AudioDuration: double read GetAudioDuration write SetAudioDuration;
    property AudioLooped: boolean read GetAudioLooped write SetAudioLooped;
  end;


var
  frmEdit: TfrmEdit;

const
  TYPE_DURATION = 0;
  TYPE_BY = 1;

implementation

uses
  main, timerframe;

{$R *.lfm}

{ TfrmEdit }

procedure TfrmEdit.FormCreate(Sender: TObject);
begin
  FProceed := False;

  with GlobalUserConfig do
  begin
    FDuration := DefaultTimerDuration;
    FDescription := DefaultTimerTitle;
    FModalAlert := ShowModalAlert;
    FTrayNotification := ShowTrayAlert;
  end;

  lblLengthText.Visible := False;
  lblLenthVal.Visible := False;
  bbSave.Enabled := Validate;

  if not TAudio.Loaded then
  begin
    bbSelectAudioFile.Enabled := False;
  end;

  FAudioInfo.FileName := '';
  FAudioInfo.Duration := 0;
  FAudioInfo.Looped := False;

end;

procedure TfrmEdit.bbSaveClick(Sender: TObject);
begin
  FDescription := edtDescription.Text;

  FDuration := dtpDuration.Time;
  FModalAlert := ckbModalAlert.Checked;
  FTrayNotification := ckbTrayNotification.Checked;
  FUseDefaultSound:=ckbUseDefaultSound.Checked;
  FProceed := True;
  Close;
end;

procedure TfrmEdit.bbSelectAudioFileClick(Sender: TObject);
var
  FileName: string;
begin
  odgAudio.InitialDir := '.';
  odgAudio.Options := [ofFileMustExist];

  if odgAudio.Execute then
  begin
    FileName := odgAudio.FileName;
    AudioFileName := FileName;
  end;
end;

procedure TfrmEdit.ckbUseDefaultSoundChange(Sender: TObject);
var
  DefaultSoundOff: boolean;
begin
  DefaultSoundOff:= not ckbUseDefaultSound.Checked;
  bbSelectAudioFile.Enabled:= DefaultSoundOff;
  bbClearAudioFile.Enabled:= DefaultSoundOff;
  //ckbLoop.Enabled:=DefaultSoundOff;
end;

procedure TfrmEdit.dtpByChange(Sender: TObject);
begin
  bbSave.Enabled := Validate;
end;

procedure TfrmEdit.dtpDurationChange(Sender: TObject);
begin
  bbSave.Enabled := Validate;
end;

procedure TfrmEdit.edtDescriptionChange(Sender: TObject);
begin
  bbSave.Enabled := Validate;
end;

procedure TfrmEdit.bbCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmEdit.bbClearAudioFileClick(Sender: TObject);
begin
  AudioFileName := '';
  edtAudioFile.Text := '';
end;

procedure TfrmEdit.FormDestroy(Sender: TObject);
begin
  //FSpecs.Free;
end;

procedure TfrmEdit.FormShow(Sender: TObject);
begin

end;

function TfrmEdit.Validate: boolean;
var
  Hours, Minutes, Seconds: word;
begin
  if edtDescription.Text = '' then
  begin
    Result := False;
    Exit;
  end;

  Hours := HourOf(dtpDuration.Time);
  Minutes := MinuteOf(dtpDuration.Time);
  Seconds := SecondOf(dtpDuration.Time);

  if ((Hours = 0) and (Minutes = 0) and (Seconds = 0)) then
  begin
    Result := False;
    Exit;
  end;

  Result := True;

end;


procedure TfrmEdit.SetDescription(AValue: string);
begin
  FDescription := AValue;
  edtDescription.Text := AValue;
end;

procedure TfrmEdit.SetAudioFileName(AValue: string);
begin
  if TAudio.Loaded then
  begin
    //Audio.FileName := AValue;
    try
      Audio.LoadFromFile(AValue);
    except
      on E: EInvalidAudio do
      begin
        //edtAudioFile.text:='';
        //lblLenthVal.Caption:='';
        MessageDlg('Cannot load file as audio. Invalid format.'
          + LineEnding + AValue, mtError, [mbOK], 0);
        Exit;
      end;
    end;
    edtAudioFile.Text := Audio.FileName;
    lblLenthVal.Caption := FloatToStr(RoundTo(Audio.Duration, -2));
    //AudioDuration := Audio.AudioFile.Duration;
  end
  else
  begin
    FAudioInfo.FileName := AValue;
  end;

  if AValue = '' then
  begin
    lblLengthText.Visible := False;
    edtAudioFile.Text := '';

    lblLenthVal.Visible := False;
    Exit;
  end    //TODO: What is the else part ?
  else
  begin
    lblLengthText.Visible := True;
    edtAudioFile.Text := AudioFileName;
    lblLenthVal.Visible := True;
  end;
end;

procedure TfrmEdit.SetAudioLooped(AValue: boolean);
begin
  if TAudio.Loaded then
    Audio.Looped := AValue
  else
    FAudioInfo.Looped := AValue;

  ckbLoop.Checked := AudioLooped;
end;

function TfrmEdit.GetAudioFileName: string;
begin
  if TAudio.Loaded then
    Result := Audio.FileName
  else
    Result := FAudioInfo.FileName;
end;

function TfrmEdit.GetAudioDuration: double;
begin
  if TAudio.Loaded then
    Result := Audio.Duration
  else
    Result := FAudioInfo.Duration;
end;

function TfrmEdit.GetAudioLooped: boolean;
begin
  if TAudio.Loaded then
    Result := Audio.Looped
  else
    Result := FAudioInfo.Looped;
end;

procedure TfrmEdit.SetAudio(AValue: TAudio);
begin
  FAudio := AValue;
  if FAudio = nil then
    Exit;
  if FAudio.FileName = '' then
  begin
    lblLengthText.Visible := False;
    edtAudioFile.Text := '';

    lblLenthVal.Visible := False;
    Exit;
  end
  else
  begin
    lblLengthText.Visible := True;
    edtAudioFile.Text := AudioFileName;
    lblLenthVal.Caption := FloatToStr(RoundTo(FAudio.Duration, -2));
    lblLenthVal.Visible := True;
  end;
end;

procedure TfrmEdit.SetAudioDuration(AValue: double);
begin
  if not TAudio.Loaded then
  begin
    FAudioInfo.Duration := AValue;
    lblLenthVal.Caption := FloatToStr(RoundTo(FAudioInfo.Duration, -2));
  end;
end;

procedure TfrmEdit.SetDuration(AValue: TTime);
begin
  FDuration := AValue;
  dtpDuration.Time := AValue;
end;

procedure TfrmEdit.SetFTrayNotification(AValue: boolean);
begin
  FTrayNotification := AValue;
  ckbTrayNotification.Checked := AValue;
end;

procedure TfrmEdit.SetModalAlert(AValue: boolean);
begin
  FModalAlert := AValue;
  ckbModalAlert.Checked := AValue;
end;

procedure TfrmEdit.SetUseDefaultSound(AValue: boolean);
begin
  FUseDefaultSound:=AValue;
  ckbUseDefaultSound.Checked:=AValue;
  bbClearAudioFile.Enabled:=(not AValue);
  bbSelectAudioFile.Enabled:=(not AValue);
  ckbLoop.Enabled:=(not AValue);

end;

function TfrmEdit.ShowAndGetSpecs: boolean;
begin
  FProceed := False;

  ShowModal;
  Result := FProceed;
end;

function TfrmEdit.ShowForAdd: boolean;
begin
  Caption := 'Add Timer';
  tsTimer.Show;
  FId := longword(-1);
  Result := ShowAndGetSpecs;
end;

function TfrmEdit.ShowForEdit(Sender: TFrame): boolean;
var
  Widget: TfraTimer;
  ButtonStatus: boolean;
begin
  if Sender = nil then
    Exit;
  tsTimer.Show;
  Widget := TfraTimer(Sender);
  Fid := Widget.Id;
  Caption := 'Edit Timer';

  { When we are showing the form to edit the timer, some controls
  are disabled. For example, you cannot change the duration of the timer,
  nor can you change the audio/sound details }
  ButtonStatus:=(not Widget.Running);
  dtpDuration.Enabled := ButtonStatus;
  ckbUseDefaultSound.Enabled:=ButtonStatus;

  bbSelectAudioFile.Enabled:=ButtonStatus and (not ckbUseDefaultSound.Checked);
  bbClearAudioFile.Enabled:=ButtonStatus and (not ckbUseDefaultSound.Checked);
  ckbLoop.Enabled:=ButtonStatus and (not ckbUseDefaultSound.Checked);

  Result := ShowAndGetSpecs;
  FId := longword(-1);
end;

end.
