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
  ComCtrls, DateTimePicker, settings, dateutils, sndfile, ctypes, LazLogger,
  Math, audio;

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
    Label5: TLabel;
    lblLengthText: TLabel;
    lblLenthVal: TLabel;
    odgAudio: TOpenDialog;
    tsAudio: TPageControl;
    tsTimer: TTabSheet;
    TabSheet2: TTabSheet;
    procedure bbCancelClick(Sender: TObject);
    procedure bbClearAudioFileClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure bbSelectAudioFileClick(Sender: TObject);
    procedure dtpByChange(Sender: TObject);
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
    //FAudioFile: string;
    //FAudioLength: double;
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
    function Validate: boolean;
    //function VerifyAudioFile(const FileName: string): boolean;
  public

    function ShowAndGetSpecs: boolean;
    function ShowForAdd :boolean;
    function ShowForEdit (Sender: TFrame):boolean;
    //property Specs: TTimerSpecs read FSpecs;
    property Duration: TTime read FDuration write SetDuration;
    property Description: string read FDescription write SetDescription;
    property ModalAlert: boolean read FModalAlert write SetModalAlert;
    //property AudioFile: string read FAudioInfo.FileName;
    //property AudioLength: double read FAudioLength.Dur;
    property TrayNotification: boolean read FTrayNotification write SetFTrayNotification;
    property Id: longword read FId;
    //function SetAudioFile(AValue: string; out Error: string): boolean;
    property Audio:TAudio read FAudio write SetAudio;
    property AudioFileName:string read GetAudioFileName write SetAudioFileName;
    property AudioDuration:double read GetAudioDuration write SetAudioDuration;
    property AudioLooped:boolean read GetAudioLooped write SetAudioLooped;
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

{ TfrmEdit }

procedure TfrmEdit.FormCreate(Sender: TObject);
begin
  FProceed:=False;

  //FSpecs:=TTimerSpecs.Create();
{  FSpecs.DurationHours:=0;
  FSpecs.DurationMinutes:=10;
  FSpecs.DurationSeconds:=0;
  FSpecs.Description:='Countdown Timer';}
  with GlobalUserConfig do
  begin
    FDuration:=DefaultTimerDuration;//EncodeTime(DefaultTimerHours, DefaultTimerMins, DefaultTimerSecs,0);
    FDescription:=DefaultTimerTitle;
    FModalAlert:=ShowModalAlert;
    FTrayNotification:=ShowTrayAlert;
  end;

  lblLengthText.Visible:=False;
  lblLenthVal.Visible:=False;
  bbSave.Enabled:=Validate;

  if not TAudio.Loaded then
  begin
    bbSelectAudioFile.Enabled:=False;
  end;

  FAudioInfo.FileName:='';
  FAudioInfo.Duration:=0;
  FAudioInfo.Looped:=False;

end;

procedure TfrmEdit.bbSaveClick(Sender: TObject);
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

procedure TfrmEdit.bbSelectAudioFileClick(Sender: TObject);
var
  FileName: string;
  ErrorText: string;

begin
  odgAudio.InitialDir:='.';
  odgAudio.Options := [ofFileMustExist];
  //odgAudio.Filter:='Supported audio files|*.wav;*.ogg|All files|*.*';
  if odgAudio.Execute then
  begin
     FileName:=odgAudio.FileName;
     AudioFileName:=FileName;
     {if not SetAudioFile(FileName, ErrorText) then
     begin
       ShowMessage(ErrorText);
       end;}

  end;
  //ShowMessage(FileName);
end;

procedure TfrmEdit.dtpByChange(Sender: TObject);
begin
  bbSave.Enabled:=Validate;
end;

procedure TfrmEdit.dtpDurationChange(Sender: TObject);
begin
  bbSave.Enabled:=Validate;
end;

procedure TfrmEdit.edtDescriptionChange(Sender: TObject);
begin
  bbSave.Enabled:=Validate;
end;

procedure TfrmEdit.bbCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmEdit.bbClearAudioFileClick(Sender: TObject);
var
  ErrorText: string;
begin
  //SetAudioFile('', ErrorText);
  AudioFileName:='';
  edtAudioFile.Text:='';
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


procedure TfrmEdit.SetDescription(AValue: string);
begin
  FDescription:=AValue;
  edtDescription.Text:=AValue;
end;

procedure TfrmEdit.SetAudioFileName(AValue: string);
begin
  if TAudio.Loaded then
  begin
    Audio.FileName:=AValue;
    edtAudioFile.Text:=Audio.FileName;
    lblLenthVal.Caption:=FloatToStr(RoundTo(AudioDuration, -2));
  end
  else
  begin
    FAudioInfo.FileName:=AValue;
  end;
  if AValue = '' then
  begin
    //FAudioFile:='';
    //FAudioLength:=0;;
    lblLengthText.Visible:=False;
    edtAudioFile.Text:='';
    //lblLenthVal.Caption:=FloatToStr(RoundTo(FAudioLength, -2));
    lblLenthVal.Visible:=False;
    Exit;
  end
  else
  begin
    lblLengthText.Visible:=True;
    edtAudioFile.Text:=AudioFileName;
    lblLenthVal.Visible:=True;
  end;
end;

procedure TfrmEdit.SetAudioLooped(AValue: boolean);
begin
  if TAudio.Loaded then
   Audio.Looped := AValue
 else
   FAudioInfo.Looped := AValue;

 ckbLoop.Checked:=AudioLooped;
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
  FAudio:=AValue;
   if FAudio.FileName = '' then
   begin
     //FAudioFile:='';
     //FAudioLength:=0;;
     lblLengthText.Visible:=False;
     edtAudioFile.Text:='';
     //lblLenthVal.Caption:=FloatToStr(RoundTo(FAudio.Duration, -2));
     lblLenthVal.Visible:=False;
     Exit;
   end
   else
   begin
     lblLengthText.Visible:=True;
     edtAudioFile.Text:=AudioFileName;
     lblLenthVal.Caption:=FloatToStr(RoundTo(FAudio.Duration, -2));
     lblLenthVal.Visible:=True;
   end;
end;

procedure TfrmEdit.SetAudioDuration(AValue: double);
begin
   if not TAudio.Loaded then
   begin
    FAudioInfo.Duration := AValue;
    lblLenthVal.Caption:=FloatToStr(RoundTo(AudioDuration, -2));
   end;
end;

{function TfrmEdit.SetAudioFile(AValue: string; out Error: string): boolean;
var
  Info: SF_INFO;
  SoundFile: PSndFile;
begin
  Result := False;
  Info.format := 0;

  if AValue = '' then
  begin
    //FAudioFile:='';
    //FAudioLength:=0;;
    lblLengthText.Visible:=False;
    edtAudioFile.Text:='';
    //lblLenthVal.Caption:=FloatToStr(RoundTo(FAudioLength, -2));
    lblLenthVal.Visible:=False;
    Result := True;
    Exit;
  end;

  if not TAudio.Loaded then
  begin
    AudioInfo.FileName:=AValue;
    //AudioInfo.Duration:=AudioLength;
    edtAudioFile.Text:=AudioInfo.FileName;
    Result := True;
    Exit;
  end;

  SoundFile := sf_open(PChar(AValue), SFM_READ, @Info);
  if (SoundFile = nil) then
  begin
    DebugLn('Error in sf_open');
    //sf_perror(nil);
    //ReadKey;
    //exit;
    Error:='SoundFile is nil';
    Exit;
  end;
  DebugLn(IntToHex(Info.format, 8));
  DebugLn(IntToStr(Info.channels));
  DebugLn(IntToStr(Info.frames));
  DebugLn(IntToStr(Info.samplerate));
  DebugLn(IntToStr(Info.sections));
  FAudioLength:=(Info.frames) / (Info.samplerate);
  //ShowMessage('length is ' + FloatToStr(AudioLength));

  FAudioFile:=AValue;

  sf_close(SoundFile);


    lblLengthText.Visible:=True;
    edtAudioFile.Text:=FAudioFile;
    lblLenthVal.Caption:=FloatToStr(RoundTo(FAudioLength, -2));
    lblLenthVal.Visible:=True;
    Result := True;

end;}

procedure TfrmEdit.SetDuration(AValue: TTime);
begin
  FDuration:=AValue;
  dtpDuration.Time:=AValue;
end;

procedure TfrmEdit.SetFTrayNotification(AValue: boolean);
begin
  //if FTrayNotification=AValue then Exit;
  FTrayNotification:=AValue;
  ckbTrayNotification.Checked:=AValue;
end;

procedure TfrmEdit.SetModalAlert(AValue: boolean);
begin
  //if FModalAlert=AValue then Exit;
  FModalAlert:=AValue;
  ckbModalAlert.Checked:=AValue;
end;

function TfrmEdit.ShowAndGetSpecs: boolean;
begin
  FProceed:=False;
  {edtDescription.Text:=FSpecs.Description;
  dtpDuration.Time := EncodeTime(FSpecs.DurationHours, FSpecs.DurationMinutes,  FSpecs.DurationSeconds, 0);
  ckbModalAlert.Checked:=FSpecs.ModalAlert;
  ckbTrayNotification.Checked:=Specs.TrayNotification; }
  ShowModal;
  Result:=FProceed;
end;

function TfrmEdit.ShowForAdd: boolean;
begin
  Caption:='Add Timer';
  tsTimer.Show;
  FId := longword(-1);
  Result:=ShowAndGetSpecs;
end;

function TfrmEdit.ShowForEdit(Sender: TFrame): boolean;
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
