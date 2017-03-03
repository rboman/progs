function [status] = ffplay(vfile)
%
% Fonction pour afficher la video vfile avec ffplay.exe.
%
%  Luc Masset (2010)

%executable
ExecDir=GetPath('ExecDir');
exe_file=fullfile(ExecDir,'ffplay.exe');
if exist(exe_file) ~= 2,
 status=1;
 return
end

%fichier video
if exist(vfile) ~= 2,
 status=2;
 return
end

%jouer la video
st=sprintf('"%s" "%s"',exe_file,vfile);
status=dos(st);

return