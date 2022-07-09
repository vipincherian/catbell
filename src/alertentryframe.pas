unit alertentryframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ExtCtrls, timerframe, util;

type
  { TfraAlertEntry }

  TfraAlertEntry = class(TFrame)
    bbRestart: TBitBtn;
    bbStart: TBitBtn;
    stCompletedAt: TStaticText;
    stDuration: TStaticText;
    stDescription: TStaticText;
    procedure bbRestartClick(Sender: TObject);
    procedure bbStartClick(Sender: TObject);
  private
    FTimer: TfraTimer;
  public
    OnTimerRestart: TNotifyEvent;
    OnTimerStart: TNotifyEvent;
    constructor Create(AOwner: TComponent; AssocTimer: TFraTimer); overload;
    property Timer: TfraTimer read Ftimer;
  end;

implementation

{$R *.lfm}

{ TfraAlertEntry }

procedure TfraAlertEntry.bbRestartClick(Sender: TObject);
begin
  if Assigned(OnTimerRestart) then
  begin
    bbRestart.Enabled := False;
    bbStart.Enabled := False;
    OnTimerRestart(Self);
  end;
end;

procedure TfraAlertEntry.bbStartClick(Sender: TObject);
begin
  if Assigned(OnTimerStart) then
  begin
    bbRestart.Enabled := False;
    bbStart.Enabled := False;
    OnTimerStart(Self);
  end;
end;

constructor TfraAlertEntry.Create(AOwner: TComponent; AssocTimer: TFraTimer);
begin
  inherited Create(AOwner);

  Ftimer := AssocTimer;

  Parent := TWinControl(AOwner);

  OnTimerRestart := nil;
  OnTimerStart := nil;

  { Dynamically spacing controls }

  with UserInterfaceMetrics do
  begin
    bbStart.BorderSpacing.Top := Padding;
    bbStart.BorderSpacing.Right := Padding;
    stCompletedAt.BorderSpacing.Right := Padding;
    stDuration.BorderSpacing.Right := Padding;
    stDescription.BorderSpacing.Left := Padding;
    stDescription.BorderSpacing.Right := Padding;
  end;
end;

end.
