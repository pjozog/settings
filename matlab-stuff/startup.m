run('~/documents/MATLAB/vlfeat-0.9.13/toolbox/vl_setup.m');
addpath(genpath('~/perl/perl-svn/perls/matlab/van'));
addpath('~/perl/perl-svn/perls/matlab/');
addpath('~/documents/MATLAB/sift');
addpath('~/documents/MATLAB/toolbox_calib');
addpath('~/documents/MATLAB/misc');
addpath('~/perl/perl-svn/sba-eval/matlab/kuka');
addpath('~/perl/perl-svn/sba-eval/matlab/utils');
addpath('~/perl/perl-svn/sba-eval/matlab/kuka/postProcess');

% lcm stuff
import java.io.*;
eval(['javaaddpath ',sprintf('%s/perls/third-party/build/lcm-1.1.1/lcm-java/lcm.jar', getenv ('HOME'))])
eval(['javaaddpath ',sprintf('%s/perls/build/share/java/perls_lcmtypes.jar', getenv ('HOME'))])

% increase default text size
set (0,'DefaultAxesFontSize', 20)
set (0,'DefaultTextFontSize', 20)
