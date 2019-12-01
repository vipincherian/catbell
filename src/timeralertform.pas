unit timeralertform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType, ExtCtrls;

type

  { TfrmTimerAlert }

  TfrmTimerAlert = class(TForm)
    stxMessage: TStaticText;
    stxAdditional: TStaticText;
    FTimer: TTimer;
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure stxAdditionalClick(Sender: TObject);
    procedure stxMessageClick(Sender: TObject);
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
  BorderStyle := bsNone;
  OnShow := @FormShow;
end;

procedure TfrmTimerAlert.FormShow(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  //FTimer.Interval:=2000;
end;

procedure TfrmTimerAlert.stxAdditionalClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmTimerAlert.stxMessageClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmTimerAlert.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_THICKFRAME;
end;

procedure TfrmTimerAlert.FormClick(Sender: TObject);
begin
  Close;
end;

end.

