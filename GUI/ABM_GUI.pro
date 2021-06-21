#CONFIG      += uitools
CONFIG += release

#INCLUDEPATH  += C:/VTK-VS10/include/vtk-5.10
INCLUDEPATH  += C:/qwt-5.2.1/src
INCLUDEPATH  += D:/ffmpeg/include

FORMS         = ABM_GUI.ui #SimpleView3DUI.ui SimpleView2DUI.ui
HEADERS       = mainwindow.h qmylabel.h params.h plot.h log.h misc.h \  #myvtk.h
                libcart.h result_set.h graphs.h transfer.h profile.h \
                \   # ImageSave.h #SimpleView3DUI.h SimpleView2DUI.h
                qmycheckbox.h qmygroupbox.h
RESOURCES     += icons.qrc
SOURCES       = main.cpp mainwindow.cpp params.cpp plot.cpp \
                misc.cpp lognormal.cpp graphs.cpp \ # myvtk.cpp
                #ImageSave.cpp SimpleView3DUI.cxx SimpleView2DUI.cxx qvideooutput.cpp
                information.cpp

# See cmake_link_command.txt for the full list of libraries that CMake links
#LIBS += -LC:/VTK-VS10/lib/vtk-5.10 -lQVTK -lvtkRendering -lvtkGraphics -lvtkImaging -lvtkIO -lvtkFiltering -lvtkCommon \
#-lvtkpng -lvtktiff -lvtkjpeg -lvtkexpat -lvfw32 -lopengl32 -lwsock32 -lvtksys -lws2_32 -lvtkexoIIc -lvtkNetCDF \
#-lvtklibxml2 -lvtkzlib -lvtkalglib -lgdi32 -lkernel32 -luser32 -lgdi32 -lwinspool -lshell32 -lole32 \
#-loleaut32 -luuid -lcomdlg32 -ladvapi32
# -lVPIC -lCosmo -lpthread

#LIBS += -LC:\users\gib\abm\build32msvs\release -lpara32
LIBS += -LC:/bin -lqwt5
LIBS += -LC:/bin -lcart

QMAKE_LIBDIR += D:/ffmpeg/lib
win32:LIBS += avcodec.lib
win32:LIBS += avdevice.lib
win32:LIBS += avfilter.lib
win32:LIBS += avformat.lib
win32:LIBS += avutil.lib
win32:LIBS += postproc.lib
win32:LIBS += swresample.lib
win32:LIBS += swscale.lib

QT           += network

# install
target.path = .
sources.files = $$SOURCES $$HEADERS $$RESOURCES $$FORMS
sources.path = .
INSTALLS += target sources

DEFINES += _CRT_SECURE_NO_WARNINGS

QMAKE_LFLAGS += /OPT:NOREF
