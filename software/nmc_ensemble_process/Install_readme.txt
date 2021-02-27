
快速安装说明:

1、nmc_ensemble_process下为各阶段的后处理程序:
  00_data_prepare主要用于将EC集合预报GRIB文件转换为NC文件;
  01_data_retrieval下为各集合模式的处理程序;
  02_data_postprocess为制作集合预报衍生产品的制作程序.

2、按照配置支撑环境:
1）安装IDL8.5(安装文件为IDL8.5.rar), 将IDL的运行程序目录加入环境变量
    PATH = C:\Program Files\Exelis\IDL85\bin\bin.x86_64;...
2）配置toolsUI, 将toolsUI-4.3.rar解压缩, 并将其路径加入环境变量中UNIDATA_CLASSPATH, 如
    UNIDATA_CLASSPATH=C:\soft\toolsUI-4.3;
3）配置GRIB_API工具, 将grib_api.rar解压缩, 如C:\soft\grib_api, 然后设置环境变量,
            设置 GRIB_HOME=/cygdrive/c/soft/grib_api, 这里为的grib_api安装目录的cygwin路径形式
             将bin执行文件夹C:\soft\grib_api\bin放到环境变量的PATH路径下.

3、配置集合预报后台处理程序，详见各处理程序配置文件.

4、设置计划任务，需要为每个sav处理程序设置一个计划任务，以便自动作业.

5、将客户端工具箱升级至1.2