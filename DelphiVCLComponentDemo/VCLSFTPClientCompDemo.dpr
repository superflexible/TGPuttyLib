program VCLSFTPClientCompDemo;



uses
  Vcl.Forms,
  VCLSFTPClientComponentMainForm in 'VCLSFTPClientComponentMainForm.pas' {VCLSFTPClientComponentDemoForm},
  tgputtysftp in '..\tgputtysftp.pas',
  tgputtylib in '..\tgputtylib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVCLSFTPClientComponentDemoForm, VCLSFTPClientComponentDemoForm);
  Application.Run;
end.
