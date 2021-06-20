unit log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EventLog, LCLIntf, LCLType;
//const
  {LOG_NONE = 0;
  LOG_ERROR = 1;
  LOG_WARN = 2;
  LOG_INFO = 3;
  LOG_ALL = 4;}
type
  TLogLevel = (LOG_NONE, LOG_ERROR, LOG_WARN, LOG_INFO, LOG_ALL);
  { TLogger }

  TLogger = class(TObject)
  private
    FLogger: TEventLog;
    FLogLock: TCriticalSection;
    {%H-}constructor Create;
    {%H-}destructor {%H-}Destroy; override;
  public
    Level: TLogLevel;
    procedure Info(const Msg: string);
    procedure Info(const Fmt: string; Args: array of const);
    procedure Debug(const Msg: string);
    procedure Debug(const Fmt: string; Args: array of const);
    procedure Warning(const Msg: string);
    procedure Warning(const Fmt: string; Args: array of const);
    procedure Error(const Msg: string);
    procedure Error(const Fmt: string; Args: array of const);

  end;

var
  Logger: TLogger = nil;

implementation

{ TLogger }


constructor TLogger.Create;
begin
  InitializeCriticalSection(FLogLock);
  FLogger := TEventLog.Create(nil);
  //Logger := frmMain.evlMain;
  FLogger.LogType := ltFile;
  FLogger.FileName := 'catbell.log';
  Level := LOG_ALL;
end;

destructor TLogger.Destroy;
begin
  FLogger.Free;
  DeleteCriticalSection(FLogLock);
  inherited Destroy;
end;

procedure TLogger.Info(const Msg: string);
begin
  if Level <= LOG_INFO then
    Exit;
  EnterCriticalSection(FLogLock);
  try
    FLogger.Info(Msg);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TLogger.Info(const Fmt: string; Args: array of const);
begin
  if Level <= LOG_INFO then
    Exit;
  EnterCriticalSection(FLogLock);
  try
    FLogger.Info(Fmt, Args);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TLogger.Debug(const Msg: string);
begin
  if Level <= LOG_ALL then
    Exit;
  EnterCriticalSection(FLogLock);
  try
    FLogger.Debug(Msg);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TLogger.Debug(const Fmt: string; Args: array of const);
begin
  if Level <= LOG_ALL then
    Exit;
  EnterCriticalSection(FLogLock);
  try
    FLogger.Debug(Fmt, Args);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TLogger.Warning(const Msg: string);
begin
  if Level <= LOG_WARN then
    Exit;
  EnterCriticalSection(FLogLock);
  try
    FLogger.Warning(Msg);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TLogger.Warning(const Fmt: string; Args: array of const);
begin
  if Level <= LOG_WARN then
    Exit;
  EnterCriticalSection(FLogLock);
  try
    FLogger.Warning(Fmt, Args);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TLogger.Error(const Msg: string);
begin
  if Level <= LOG_ERROR then
    Exit;
  EnterCriticalSection(FLogLock);
  try
    FLogger.Error(Msg);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TLogger.Error(const Fmt: string; Args: array of const);
begin
  if Level <= LOG_ERROR then
    Exit;
  EnterCriticalSection(FLogLock);
  try
    FLogger.Error(Fmt, Args);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

initialization
  Logger := TLogger.Create;

finalization
  Logger.Free;
end.
