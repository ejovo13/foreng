add_library(foreng_env OBJECT foreng_env.f90)
add_library(foreng foreng.f90)

add_subdirectory(math)
add_subdirectory(io)
add_subdirectory(numeric)
add_subdirectory(science)

set(foreng_libs foreng foreng_math foreng_io foreng_numeric foreng_science foreng_env)

target_link_libraries(${foreng_libs})

include(GNUInstallDirs)


install(TARGETS ${foreng_libs}
        EXPORT forengTargets # Associate target with export name
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        INCLUDES DESTINATION include
)

install(EXPORT forengTargets
        FILE forengTargets.cmake
        NAMESPACE foreng::
        DESTINATION /usr/local/lib/cmake/foreng)



install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
        DESTINATION include)

include(CMakePackageConfigHelpers)

configure_package_config_file(${CMAKE_CURRENT_SOURCE_DIR}/Config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/forengConfig.cmake"
    INSTALL_DESTINATION /usr/local/lib/cmake/foreng
)

install(FILES

        "${CMAKE_CURRENT_BINARY_DIR}/forengConfig.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/forengConfigVersion.cmake"
        DESTINATION /usr/local/lib/cmake/foreng
)

set(version 1.0)

set_property(TARGET foreng PROPERTY VERSION ${version})
set_property(TARGET foreng PROPERTY SOVERSION 1)
set_property(TARGET foreng PROPERTY INTERFACE_foreng_MAJOR_VERSION 1)
set_property(TARGET foreng APPEND PROPERTY COMPATIBLE_INTERFACE_STRING foreng_MAJOR_VERSION)

write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/forengConfigVersion.cmake"
VERSION "${version}"
COMPATIBILITY AnyNewerVersion)