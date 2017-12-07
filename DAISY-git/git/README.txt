程序采用cmake进行编译，由于函数库的限制，当前支持VS+Intel(Win32), VS+Intel(Win64), LINUX64(ifort)。除HDF5外的库既可以用预先编译的二进制文件，也可以直接从源文件进行编译。
-->LINUX64:
export FC=ifort
mkdir build & cd build
cmake dir-to-CMakeLists.txt

-->Win:
mkdir build & cd build
cmake -G "platform" dir-to-CMakeLists.txt
指定不同的Platform即可生成不同VS版本的工程文件，其中platform可以取：
Visual Studio 11 2012               : 32位的VS2012工程文件
Visual Studio 11 2012 Win64         : 64位的VS2012工程文件
Visual Studio 12 2013
Visual Studio 12 2013 Win64

================================================================================
除此之外可以手动建立工程文件，进行编译：
1, 根据系统，选择hdf5-1.8.16-win32-vs2013-shared.zip或者hdf5-1.8.16-win64-vs2013-shared.zip，将安装目录bin添加到path路径，该函数库为程序运行所需；
2, 根据系统，选择HDFView-2.11-win32-vs2012.zip或者HDFView-2.11-win64-vs2012.zip，该工具为数据处理所需；
3, 新建工程文件，将/src, /lib-32bit, lib-64bit放置到工程的.vfproj同一目录；
4, 向工程中添加/src中的源文件；
5, 设置工程属性；
6, 编译即可；

工程属性设置：
1, 对于Win32平台下的Debug和Release进行设置：
-->Fortran-->General-->Additional Include Directories: .\lib-32bit\mathlib\include; .\lib-32bit\xmlfox\include; .\lib-32bit\hdf5\include; .\lib-32bit\lapack\include; .\lib-32bit\refprop\include;
-->Linker-->General-->Additional Library Directories: .\lib-32bit\mathlib\lib; .\lib-32bit\xmlfox\lib; .\lib-32bit\hdf5\lib; .\lib-32bit\lapack\lib; .\lib-32bit\refprop\lib;
-->Linker-->Input-->Additional Dependencies: mathlib.lib refprop.lib blas.lib lapack.lib fox_common.lib fox_dom.lib fox_fsys.lib fox_sax.lib fox_utils.lib fox_wxml.lib hdf5.lib hdf5_fortran.lib hdf5_hl.lib hdf5_hl_fortran.lib
-->Linker-->Input-->Ignore Specific Library: debug--MSVCRT.lib;
-->Fortran-->Language-->Process OpenMP Directives: /Qopenmp
-->Linker-->System-->Stack Reserve Size: 100000000
2, 新建x64模式，然后将其中的lib-32bit改为64位库所在的位置，此处为lib-64bit

假设库放在.vfproj路径上层中的src_lib中，路径配置方式为：
..\src_lib\lib-32bit\mathlib\include; ..\src_lib\lib-32bit\xmlfox\include; ..\src_lib\lib-32bit\hdf5\include; ..\src_lib\lib-32bit\lapack\include; ..\src_lib\lib-32bit\refprop\include;
..\src_lib\lib-32bit\mathlib\lib; ..\src_lib\lib-32bit\xmlfox\lib; ..\src_lib\lib-32bit\hdf5\lib; ..\src_lib\lib-32bit\lapack\lib; ..\src_lib\lib-32bit\refprop\lib;

说明：
a, HDF5版本选择1.8.16，因为该版本官网上的二进制文件支持Fortran 2003，这是当前程序必须的；
（NOTE: 最好将原来的hdf5库卸载掉，同时将新安装位置的bin目录添加到环境变量path中，如D:\Program Files\HDF_Group\HDF5\1.8.16\bin）
b, 为了方便32位以及64位系统，在工程目录文件夹下提供了32位的函数库（lib-32bit文件夹）以及64位的函数库（lib-64bit文件夹）。
如果编译64位程序，则在项目工程的配置管理器中选择x64；如果编译32位程序，则在项目工程的配置管理器中选择Win32。直接编译即可，库的路径均已经设置好。
