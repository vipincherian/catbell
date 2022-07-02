unit alertentryframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, timerframe;

type
  { TfraAlertEntry }

  TfraAlertEntry = class(TFrame)
    bbRestart: TBitBtn;
    bbStart: TBitBtn;
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
end;

end.

