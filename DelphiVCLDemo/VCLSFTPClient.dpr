program VCLSFTPClient;

uses
  Vcl.Forms,
  VCLClientMainForm in 'VCLClientMainForm.pas' {VCLSFTPClientDemoForm},
  tgputtysftp in '..\tgputtysftp.pas',
  tgputtylib in '..\tgputtylib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVCLSFTPClientDemoForm, VCLSFTPClientDemoForm);
  Application.Run;
end.
