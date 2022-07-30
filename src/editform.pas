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
unit editform;

{$mode objfpc}{$H+}{$Q+}{$R+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, DateTimePicker, settings, dateutils, {sndfile, ctypes,} {EventLog,}
  Math, audio, log{, sound}, constants, util;

type

  { TfrmEdit }

  TfrmEdit = class(TForm)
    bbCancel: TBitBtn;
    bbClearSound: TBitBtn;
    bbSave: TBitBtn;
    bbSelectSound: TBitBtn;
    bbStopSound: TBitBtn;
    bbTestSound: TBitBtn;
    ckbLoop: TCheckBox;
    ckbMetronome: TCheckBox;
    ckbModalAlert: TCheckBox;
    ckbTrayNotification: TCheckBox;
    cmbSoundType: TComboBox;
    dtpDuration: TDateTimePicker;
    edtDescription: TEdit;
    gbSpecs: TGroupBox;
    gbOnCompletion: TGroupBox;
    gbAlarm: TGroupBox;
    gbCustom: TGroupBox;
    gbMetronome: TGroupBox;
    imlEdit: TImageList;
    lblDescription: TLabel;
    lblDuration: TLabel;
    lsvSoundDetails: TListView;
    odgAudio: TOpenDialog;
    pgEditTimer: TPageControl;
    tsTimer: TTabSheet;
    tsSound: TTabSheet;
    procedure bbCancelClick(Sender: TObject);
    procedure bbClearSoundClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure bbSelectSoundClick(Sender: TObject);
    procedure bbStopSoundClick(Sender: TObject);
    procedure bbTestSoundClick(Sender: TObject);
    procedure ckbUseDefaultSoundChange(Sender: TObject);
    procedure cmbSoundTypeChange(Sender: TObject);
    procedure cmbSoundTypeEnter(Sender: TObject);
    procedure dtpByChange(Sender: TObject);
    procedure dtpDurationChange(Sender: TObject);
    procedure edtDescriptionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FProceed: boolean;
    //FId: longword;
    FWidget: Pointer;

    FAudioPlayer: TAudioPlayer;

    FSoundIndex: integer;
    FLoadedSoundIndex: integer;
    FLoadedSoundSource: string;

    FSoundTypePrevious: integer;

    function GetDescription: string;
    function GetDuration: TTime;
    function GetMetronome: boolean;
    function GetModalAlert: boolean;
    //function GetSoundDuration: double;
    function GetSoundLooped: boolean;
    function GetTrayNotification: boolean;
    function GetUseDefaultSound: boolean;
    procedure LayoutControls;
    procedure SetLoadedSoundIndex(AValue: integer);
    procedure SetLoadedSoundSource(AValue: string);
    procedure SetMetronome(AValue: boolean);

    procedure SetSoundIndex(AValue: integer);

    procedure SetSoundLooped(AValue: boolean);
    procedure SetDescription(AValue: string);
    procedure SetDuration(AValue: TTime);
    procedure SetFTrayNotification(AValue: boolean);
    procedure SetModalAlert(AValue: boolean);
    procedure SetUseDefaultSound(AValue: boolean);
    function Validate: boolean;
    procedure AudioPlayed(Sender: TObject);
  public
    function ShowAndGetSpecs: boolean;
    function ShowForAdd: boolean;
    function ShowForEdit(Sender: TFrame): boolean;
    { Single place where controls are enabled/disabled }
    procedure ReenableControls;
    function LoadSoundFile: boolean;

    property Duration: TTime read GetDuration write SetDuration;
    property Description: string read GetDescription write SetDescription;
    property ModalAlert: boolean read GetModalAlert write SetModalAlert;

    property TrayNotification: boolean read GetTrayNotification
      write SetFTrayNotification;
    property SoundIndex: integer read FSoundIndex write SetSoundIndex;
    property LoadedSoundIndex: integer read FLoadedSoundIndex write SetLoadedSoundIndex;
    property LoadedSoundSource: string read FLoadedSoundSource
      write SetLoadedSoundSource;

    //property Id: longword read FId;

    property SoundLooped: boolean read GetSoundLooped write SetSoundLooped;
    property Metronome: boolean read GetMetronome write SetMetronome;
  end;


var
  frmEdit: TfrmEdit;


implementation

uses
  timerframe;

{$R *.lfm}

{ TfrmEdit }

procedure TfrmEdit.FormCreate(Sender: TObject);
begin
  FProceed := False;
  FWidget := nil;

  if AudioSystem.Loaded then
  begin
    FAudioPlayer := TAudioPlayer.Create;
    FAudioPlayer.OnPlayCompletion := @AudioPlayed;
    FSoundIndex := SoundPool.DefaultSoundIndex;
  end;
  {SetLoadedSoundIndex has a check whether the value is changing. If not,
  it does not do anything. Hence you cannot start with
  INVALID_SOUNDPOOL_INDEX}
  FLoadedSoundIndex := INVALID_SOUNDPOOL_INDEX - 1;
  LoadedSoundSource := '';

  with UserConfig do
  begin
    Duration := DefaultTimerDuration;
    Description := DefaultTimerTitle;
    ModalAlert := ShowModalAlert;
    TrayNotification := ShowTrayAlert;
  end;

  LayoutControls;

  ReenableControls;

end;

procedure TfrmEdit.bbSaveClick(Sender: TObject);
begin

  FProceed := True;
  Close;
end;

procedure TfrmEdit.bbSelectSoundClick(Sender: TObject);
begin
  if LoadSoundFile then
  begin
    FSoundIndex := FLoadedSoundIndex;
    cmbSoundType.ItemIndex := SOUND_CUSTOM;
  end;

  ReenableControls;
end;

procedure TfrmEdit.bbStopSoundClick(Sender: TObject);
begin
  FAudioPlayer.Abort;
end;

procedure TfrmEdit.bbTestSoundClick(Sender: TObject);
begin
  {If the default sound is being used, play that, and scamper}
  FAudioPlayer.Looped := ckbLoop.Checked;
  FAudioPlayer.AmplitudeScalePoller := @AudioSystem.GetAmplitudeScale;
  if (FSoundIndex >= SoundPool.DefaultSoundIndex) then
    FAudioPlayer.Play(SoundPool.RawSound[FSoundIndex]);
  ReenableControls;
end;

procedure TfrmEdit.ckbUseDefaultSoundChange(Sender: TObject);
begin
  ReenableControls;
end;

procedure TfrmEdit.cmbSoundTypeChange(Sender: TObject);
begin
  case cmbSoundType.ItemIndex of
    SOUND_CUSTOM:
    begin
      if FLoadedSoundIndex = INVALID_SOUNDPOOL_INDEX then
        if LoadSoundFile then
          FSoundIndex := FLoadedSoundIndex
        else
          cmbSoundType.ItemIndex := FSoundTypePrevious
      else
        FSoundIndex := FLoadedSoundIndex;
    end;
    SOUND_DEFAULT: FSoundIndex := SoundPool.DefaultSoundIndex;
    SOUND_NONE: FSoundIndex := INVALID_SOUNDPOOL_INDEX;
    else
      Logger.Warning('cmbSoundType.ItemIndex - ' + IntToStr(FSoundIndex) +
        ' at ' + string({$INCLUDE %FILE%}) + ':' + string({$INCLUDE %LINE%}));
  end;
  ReenableControls;
end;

procedure TfrmEdit.cmbSoundTypeEnter(Sender: TObject);
begin
  FSoundTypePrevious := cmbSoundType.ItemIndex;
end;

procedure TfrmEdit.dtpByChange(Sender: TObject);
begin
  ReenableControls;
end;

procedure TfrmEdit.dtpDurationChange(Sender: TObject);
begin
  ReenableControls;
end;

procedure TfrmEdit.edtDescriptionChange(Sender: TObject);
begin
  ReenableControls;
end;

procedure TfrmEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  StartTickCount: QWord;
begin

  StartTickCount := GetTickCount64;
  { Wait for FAudioPlayer to complete }
  if AudioSystem.Loaded then
  begin
    if FAudioPlayer.Playing then
    begin
      FAudioPlayer.Abort;
      while FAudioPlayer.Playing do
      begin
        Logger.Debug('Waiting for');
        Application.ProcessMessages;
        if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
          break;
      end;
    end;
  end;
end;

procedure TfrmEdit.bbCancelClick(Sender: TObject);
var
  StartTickCount: QWord;
begin
  FAudioPlayer.Abort;
  StartTickCount := GetTickCount64;
  { Abort is asynchronous, wait till FAudioPlayer completes it work.
  Also, we do not wait for more than two seconds per timer.
  After that, it is past caring. Tardiness can only be tolerated as much. }
  while FAudioPlayer.Playing do
  begin
    Logger.Debug('Waiting for frmEdit to stop audio');
    Application.ProcessMessages;
    if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
      break;
  end;
  Close;
end;

procedure TfrmEdit.bbClearSoundClick(Sender: TObject);
begin
  LoadedSoundIndex := INVALID_SOUNDPOOL_INDEX;
  FSoundIndex := SoundPool.DefaultSoundIndex;
  LoadedSoundSource := '';
  ReenableControls;
end;

procedure TfrmEdit.FormDestroy(Sender: TObject);
begin
  FAudioPlayer.Free;
end;

procedure TfrmEdit.FormShow(Sender: TObject);
begin
  ;
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

procedure TfrmEdit.ReenableControls;
var
  WidgetRunning, WidgetPlayingSound: boolean;
  Widget: TfraTimer;
  AudioPlaying: boolean = False;
begin
  WidgetRunning := False;
  WidgetPlayingSound := False;

  if AudioSystem.Loaded then
    AudioPlaying := FAudioPlayer.Playing;

  if FWidget <> nil then
  begin
    Widget := TfraTimer(FWidget);
    WidgetPlayingSound := Widget.IsSoundPlaying;
    WidgetRunning := Widget.Running;
  end;

  { This controls takes care of enabling/disabling controls at various
  junctures.}

  dtpDuration.Enabled := (not WidgetRunning);

  ckbLoop.Enabled := (not WidgetPlayingSound) and AudioSystem.Loaded and
    (not FAudioPlayer.Playing);

  bbSelectSound.Enabled := (not WidgetPlayingSound) and AudioSystem.Loaded and
    (not AudioPlaying);
  bbClearSound.Enabled := (not WidgetPlayingSound) and (not AudioPlaying) and
    AudioSystem.Loaded;

  cmbSoundType.Enabled := AudioSystem.Loaded;

  if AudioSystem.Loaded then
  begin
    if FSoundIndex = SoundPool.DefaultSoundIndex then
      cmbSoundType.ItemIndex := SOUND_DEFAULT
    else if FSoundIndex = INVALID_SOUNDPOOL_INDEX then
      cmbSoundType.ItemIndex := SOUND_NONE
    else if FSoundIndex >= SoundPool.CustomSoundRangeStart then
      cmbSoundType.ItemIndex := SOUND_CUSTOM
    else
      Logger.Warning('Unexpected FSoundIndex - ' + IntToStr(FSoundIndex) +
        ' at ' + string({$INCLUDE %FILE%}) + ':' + string({$INCLUDE %LINE%}));

    lsvSoundDetails.Visible := (FSoundIndex >= SoundPool.CustomSoundRangeStart);
    bbClearSound.Enabled := lsvSoundDetails.Visible;
  end;

  { This /has/ to be after cmbSoundType.ItemIndex is set, as there is a
  dependency }
  bbTestSound.Enabled := (not WidgetPlayingSound) and AudioSystem.Loaded and
    (not AudioPlaying) and (cmbSoundType.ItemIndex <> SOUND_NONE);
  bbStopSound.Enabled := AudioSystem.Loaded and AudioPlaying;



  bbSave.Enabled := Validate;

end;

function TfrmEdit.LoadSoundFile: boolean;
var
  FileName: string;
  Index: integer;
begin
  Result := False;

  odgAudio.InitialDir := '.';
  odgAudio.Options := [ofFileMustExist];

  if odgAudio.Execute then
  begin
    FileName := odgAudio.FileName;
    Index := SoundPool.LoadSoundFromFile(FileName);
    if Index <> INVALID_SOUNDPOOL_INDEX then
    begin
      LoadedSoundIndex := Index;
      LoadedSoundSource := FileName;
    end
    else
      MessageDlg('Error', 'Could not load audio from file: ' + LineEnding +
        FileName, mtError, [mbOK], 0);
  end
  else
    Exit;
  Result := True;
end;

procedure TfrmEdit.AudioPlayed(Sender: TObject);
begin
  ReenableControls;
end;

procedure TfrmEdit.SetDescription(AValue: string);
begin
  edtDescription.Text := AValue;
end;

procedure TfrmEdit.SetSoundLooped(AValue: boolean);
begin
  ckbLoop.Checked := AValue;
end;

//function TfrmEdit.GetSoundDuration: double;
//begin
//  { TODO : What is this for? }
//  Result := 0;
//end;

function TfrmEdit.GetDescription: string;
begin
  Result := edtDescription.Text;
end;

function TfrmEdit.GetDuration: TTime;
begin
  Result := dtpDuration.Time;
end;

function TfrmEdit.GetMetronome: boolean;
begin
  Result := ckbMetronome.Checked;
end;

function TfrmEdit.GetModalAlert: boolean;
begin
  Result := ckbModalAlert.Checked;
end;

function TfrmEdit.GetSoundLooped: boolean;
begin
  Result := ckbLoop.Checked;
end;

function TfrmEdit.GetTrayNotification: boolean;
begin
  Result := ckbTrayNotification.Checked;
end;

function TfrmEdit.GetUseDefaultSound: boolean;
begin
  Result := (cmbSoundType.ItemIndex = SOUND_DEFAULT);
end;

procedure TfrmEdit.LayoutControls;
begin
  { Dymamic layout of controls }
  with UserInterfaceMetrics do
  begin

    { Form: Controls on which others are anchored }
    with bbSave.BorderSpacing do
    begin
      Right := Margin;
      Bottom := Margin;
    end;
    bbCancel.BorderSpacing.Left := Margin;

    { Other controls }
    with pgEditTimer.BorderSpacing do
    begin
      Top := Padding;
      Bottom := Padding;
    end;

    {Page Timer: Controls on which others are anchored }
    gbSpecs.BorderSpacing.Around := Padding;
    gbSpecs.BorderSpacing.InnerBorder := Padding;

    with gbOnCompletion.BorderSpacing do
    begin
      Top := Padding;
      Bottom := Padding;
      InnerBorder := Padding;
    end;

    lblDescription.BorderSpacing.Left := Padding;

    with edtDescription.BorderSpacing do
    begin
      Left := Padding;
      Right := Padding;
      Top := Padding;
    end;

    {Page timer: Other controls }

    dtpDuration.BorderSpacing.Top := Padding;

    with ckbModalAlert.BorderSpacing do
    begin
      Top := Padding;
      Left := Padding;
    end;
    ckbTrayNotification.BorderSpacing.Top := Padding;

    {Page Sound: Controls on which others are anchored }

    gbMetronome.BorderSpacing.Around := Padding;
    gbMetronome.BorderSpacing.InnerBorder := Padding;
    with ckbMetronome.BorderSpacing do
    begin
      Left := Padding;
      //Top:=Margin;
      //Right:=Margin;
    end;

    with gbAlarm.BorderSpacing do
    begin
      InnerBorder := Padding;
      Top := Padding;
      //Left:=Margin;
    end;

    with bbTestSound.BorderSpacing do
    begin
      Top := Padding;
      Right := Padding;
    end;

    bbStopSound.BorderSpacing.Top := Padding;
    cmbSoundType.BorderSpacing.Left := Padding;
    cmbSoundType.BorderSpacing.Right := Padding;

    with gbCustom.BorderSpacing do
    begin
      InnerBorder := Padding;
      Left := Padding;
      Right := Padding;
    end;

    with bbClearSound.BorderSpacing do begin
      Top:=Padding;
      Right:=Padding;
    end;

    bbSelectSound.BorderSpacing.Left:=Padding;

    with lsvSoundDetails.BorderSpacing do begin
      Top:=Padding;
      Bottom:=Padding;
    end;


  end;

end;

procedure TfrmEdit.SetLoadedSoundIndex(AValue: integer);
var
  Details: TSoundPoolEntryDetails;
begin
  Assert((AValue = INVALID_SOUNDPOOL_INDEX) or
    (AValue >= SoundPool.CustomSoundRangeStart));

  if FLoadedSoundIndex = AValue then
    Exit;

  FLoadedSoundIndex := AValue;

  lsvSoundDetails.Items.BeginUpdate;
  if FLoadedSoundIndex > INVALID_SOUNDPOOL_INDEX then
  begin
    Details := SoundPool.RawSoundDetails[FLoadedSoundIndex]^;

    lsvSoundDetails.Items[2].SubItems.Clear;
    lsvSoundDetails.Items[2].SubItems.Add(
      FloatToStr(RoundTo(Details.Duration, -2)) + 's');
    lsvSoundDetails.Visible := True;
  end
  else
  begin
    lsvSoundDetails.Items[2].SubItems.Clear;
    lsvSoundDetails.Visible := False;
  end;
  lsvSoundDetails.Items.EndUpdate;
end;

procedure TfrmEdit.SetLoadedSoundSource(AValue: string);
begin
  if FLoadedSoundSource = AValue then
    Exit;
  FLoadedSoundSource := AValue;

  lsvSoundDetails.BeginUpdate;
  if FLoadedSoundSource <> '' then
  begin

    lsvSoundDetails.Items[0].SubItems.Clear;
    lsvSoundDetails.Items[0].SubItems.Add(ExtractFileName(LoadedSoundSource));
    lsvSoundDetails.Items[1].SubItems.Clear;
    lsvSoundDetails.Items[1].SubItems.Add(LoadedSoundSource);

  end
  else
  begin
    lsvSoundDetails.Items[0].SubItems.Clear;
    lsvSoundDetails.Items[1].SubItems.Clear;
  end;
  lsvSoundDetails.EndUpdate;

end;

procedure TfrmEdit.SetMetronome(AValue: boolean);
begin
  ckbMetronome.Checked := AValue;
end;

procedure TfrmEdit.SetSoundIndex(AValue: integer);
begin
  Assert(AValue >= INVALID_SOUNDPOOL_INDEX);
  FSoundIndex := Avalue;
end;

procedure TfrmEdit.SetDuration(AValue: TTime);
begin
  dtpDuration.Time := AValue;
end;

procedure TfrmEdit.SetFTrayNotification(AValue: boolean);
begin
  ckbTrayNotification.Checked := AValue;
end;

procedure TfrmEdit.SetModalAlert(AValue: boolean);
begin
  ckbModalAlert.Checked := AValue;
end;

procedure TfrmEdit.SetUseDefaultSound(AValue: boolean);
begin
  if AValue then
    cmbSoundType.ItemIndex := SOUND_DEFAULT;
  ReenableControls;

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
  //FId := longword(-1);
  ReenableControls;
  Result := ShowAndGetSpecs;
end;

function TfrmEdit.ShowForEdit(Sender: TFrame): boolean;
var
  Widget: TfraTimer;
begin
  if Sender = nil then
  begin
    Result := False;
    Exit;
  end;
  tsTimer.Show;
  Widget := TfraTimer(Sender);
  //Fid := Widget.Id;
  FWidget := Widget;
  Caption := 'Edit Timer';

  ReenableControls;

  Result := ShowAndGetSpecs;
  //FId := longword(-1);
end;

end.
