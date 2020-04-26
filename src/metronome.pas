unit metronome;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, audio;

type

  { TMetronome }

  TMetronome = class(TObject)
    private
      FAudio: TAudio;
      FTickSound: TSndSound;

    public
      constructor Create;
      destructor Destroy; override;
  end;

implementation

{ TMetronome }

constructor TMetronome.Create;
begin
  FAudio := TAudio.Create;
  FTickSound := TSndSound.Create;
  FTickSound.LoadTick;
end;

destructor TMetronome.Destroy;
begin
  FAudio.Free;
  inherited Destroy;
end;

end.

