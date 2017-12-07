fn=fopen('E:\documents\doctors degree\software\tansistant\parallel\moduledebug\assembly\debugassembly\output\mesh.txt','r');
node=fscanf(fn,'%f',[1,inf]);%Nf,Ng,Ns,Ny,M,N,Nt
fclose(fn);
Nf=node(1);Ng=node(2);Ns=node(3);Ny=node(4);
M=Ny+1;
N=Nf+Ng+Ns+1;

fid=fopen('E:\documents\doctors degree\software\tansistant\parallel\moduledebug\assembly\debugassembly\output\temperature.txt','r');
%T=textread('E:\documents\doctors degree\software\tansistant\output\temperature.txt');
T=fscanf(fid,'%f',[M-1,N]);
fclose(fid);

fpressure=fopen('E:\documents\doctors degree\software\tansistant\parallel\moduledebug\assembly\debugassembly\output\pressure.txt','r');
%T=textread('E:\documents\doctors degree\software\tansistant\output\temperature.txt');
pressure=fscanf(fpressure,'%f',[M-1,N]);
fclose(fpressure);

fvelocity=fopen('E:\documents\doctors degree\software\tansistant\parallel\moduledebug\assembly\debugassembly\output\velocity.txt','r');
%T=textread('E:\documents\doctors degree\software\tansistant\output\temperature.txt');
velocity=fscanf(fvelocity,'%f',[M-1,N]);
fclose(fvelocity);

fctime=fopen('E:\documents\doctors degree\software\tansistant\parallel\moduledebug\assembly\debugassembly\output\ctime.txt','r');
%T=textread('E:\documents\doctors degree\software\tansistant\output\temperature.txt');
ctime=fscanf(fctime,'%f',[1,inf]);
fclose(fctime);

ftout=fopen('E:\documents\doctors degree\software\tansistant\parallel\moduledebug\assembly\debugassembly\output\tout.txt','r');
%T=textread('E:\documents\doctors degree\software\tansistant\output\temperature.txt');
tout=fscanf(ftout,'%f',[1,inf]);
fclose(ftout);

ftpow=fopen('E:\documents\doctors degree\software\tansistant\parallel\moduledebug\assembly\debugassembly\output\tpow.txt','r');
%T=textread('E:\documents\doctors degree\software\tansistant\output\temperature.txt');
tpow=fscanf(ftpow,'%f',[1,inf]);
fclose(ftpow);

ftuin=fopen('E:\documents\doctors degree\software\tansistant\parallel\moduledebug\assembly\debugassembly\output\tuin.txt','r');
%T=textread('E:\documents\doctors degree\software\tansistant\output\temperature.txt');
tuin=fscanf(ftuin,'%f',[1,inf]);
fclose(ftuin);
Nt=length(ctime)
x=1:1:M-2
subplot(2,3,5);plot(x,velocity),title('稳态工况速度沿轴向分布'),xlabel('轴向节点'),ylabel('速度/m/s')
x=1:1:M-1
subplot(2,3,4);plot(x,pressure),title('稳态工况压力沿轴向分布'),xlabel('轴向节点'),ylabel('压力/Pa')
subplot(2,3,6);plot(x,T(:,N)),title('稳态工况温度沿轴向分布'),xlabel('轴向节点'),ylabel('温度/K')
y=1:1:Nt
subplot(2,3,1);plot(y,tpow),title('功率密度随时间分布'),xlabel('时间s'),ylabel('功率密度W/m3')
subplot(2,3,2);plot(y,tuin),title('入口流速随时间分布'),xlabel('时间s'),ylabel('入口流速m/s')
subplot(2,3,3);plot(y,tout),title('堆芯出口温度随时间分布'),xlabel('时间s'),ylabel('堆芯出口温度K')
