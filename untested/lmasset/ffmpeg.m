function [status] = ffmpeg(varargin)
%
% Fonction pour creer une video avec ffmpeg.exe. On donne soit une chaine de caracteres
% avec les parametres de la ligne de commandes de ffmpeg, soit plusieurs arguments en
% commencant par une string qui donne le type d'action a effectuer.
%
%  Exemple :
%
% ffmpeg('anim1','C:\temp\frame%04d.png',25,Inf,'500x474','-vcodec msmpeg4v2','C:\temp\video.avi')
%
% ce qui donne la ligne de commandes FFmpeg suivante :
%
% ffmpeg.exe -y -r 25 -sameq -f image2 -i "C:\temp\frame%04d.png" -s 500x474 -vcodec msmpeg4v2 "C:\temp\video.avi"
%
% Ici, le parametre "-vcodec msmpeg4v2" indique le codec video a utiliser pour un avi. Pour d'autres
% types de video (QuickTime, FLV, ...), on n'est pas oblige de mettre le -vcodec.
%
% Voir la doc de FFmpeg pour plus de details.
%
%  Luc Masset (2010)

%initialisation
status=1;

%try/catch
try,

%arguments
if nargin == 1,
 params=varargin{1};
else
 action=varargin{1};
 switch action,
 case 'anim1',
  gname=varargin{2};
  frate=varargin{3};
  brate=varargin{4};
  vsize=varargin{5};
  oparam=varargin{6};
  output=varargin{7};
  [p,n,ext]=fileparts(output);
  tsize='';
  if ~isempty(vsize),
   tsize=sprintf('-s %s',vsize);
  end
  if isinf(brate),
   params=sprintf(' -y -r %i -sameq -f image2 -i "%s" %s %s "%s"',frate,gname,tsize,oparam,output);
  else
   params=sprintf(' -y -r %i -vb %i -f image2 -i "%s" %s %s "%s"',frate,brate,gname,tsize,oparam,output);
  end
 case 'anim1_sound',
  gname=varargin{2};
  frate=varargin{3};
  brate=varargin{4};
  vsize=varargin{5};
  faudio=varargin{6};
  nfr=varargin{7};
  oparam=varargin{8};
  output=varargin{9};
  [p,n,ext]=fileparts(output);
  tsize='';
  if ~isempty(vsize),
   tsize=sprintf('-s %s',vsize);
  end
  if isinf(brate),
   params=sprintf(' -y -r %i -sameq -f image2 -i "%s" %s -i "%s" -vframes %i %s "%s"',frate,gname,tsize,faudio,nfr,oparam,output);
  else
   params=sprintf(' -y -r %i -vb %i -f image2 -i "%s" %s -i "%s" -vframes %i %s "%s"',frate,brate,gname,tsize,faudio,nfr,oparam,output);
  end
 otherwise
  return
 end
end

%executable
ExecDir=GetPath('ExecDir');
exe_file=fullfile(ExecDir,'ffmpeg.exe');
if exist(exe_file) ~= 2,
 status=2;
 return
end
st=sprintf('"%s" %s',exe_file,params);
status=dos(st);

%arguments
if nargin == 1,
 return
else
 action=varargin{1};
 switch action,
 case {'anim1','anim1_sound'},
  if status,
   delete1(output);
   return
  end
  if exist(output) ~= 2,
   status=3;
   return
  end
 otherwise
  return
 end
end

%try/catch
catch
 lasterr
end

return