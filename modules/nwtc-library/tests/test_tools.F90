module test_tools

    use NWTC_IO
    use NWTC_Library
    use NWTC_Library_Types

    implicit none

contains

    function getTestInputLogicals()

        TYPE(FileInfoType)                  :: getTestInputLogicals
        CHARACTER(1024), DIMENSION(2)       :: data = (/ &
            '        true  Echo           - Echo input data to <RootName>.ech (flag) ', &
            '        false Echo           - Echo input data to <RootName>.ech (flag) ' &
        /)

        CALL InitFileInfo(data, getTestInputLogicals)

    end function

    function getTestInputCharacters()

        TYPE(FileInfoType)                  :: getTestInputCharacters
        CHARACTER(1024), DIMENSION(5)       :: data = (/ &
            ! Characters
            '"Wind/08ms.wnd"        UniformFileName       - Filename of time series d', &
            'Wind/08ms.wnd          UniformFileName       - Filename of time series d', &
            '"test with spaces"     UniformFileName       - Filename of time set     ', &
            '"test with ; chars"    UniformFileName       - Filename of time set     ', &
            '"test with , chars"    UniformFileName       - Filename of time set     ' &
        /)

        CALL InitFileInfo(data, getTestInputCharacters)

    end function

    function getTestInputIntegers()

        TYPE(FileInfoType)                  :: getTestInputIntegers
        CHARACTER(1024), DIMENSION(7)       :: data = (/ &
            ! Integers
            '          1   WindType      - switch for wind file type (1=steady; 2=  ', &
            ' 1000000000   WindType      - switch for wind file type (1=steady; 2=  ', &
            '         -1   WindType      - switch for wind file type (1=steady; 2=  ', &
            '-1000000000   WindType      - switch for wind file type (1=steady; 2=  ', &
            '        1.0   WindType      - switch for wind file type (1=steady; 2=  ', &
            '       -1.0   WindType      - switch for wind file type (1=steady; 2=  ', &
            '        1.2   WindType      - switch for wind file type (1=steady; 2=  ' &
        /)

        CALL InitFileInfo(data, getTestInputIntegers)

    end function

    function getTestInputFloats()

        TYPE(FileInfoType)                  :: getTestInputFloats
        CHARACTER(1024), DIMENSION(5)       :: data = (/ &
        ! Float Types
            '        8.0   HWindSpeed     - Horizontal windspeed (m/s)               ', &
            '       -8.0   HWindSpeed     - Horizontal windspeed (m/s)               ', &
            '          8   HWindSpeed     - Horizontal windspeed (m/s)               ', &
            '          0   HWindSpeed     - Horizontal windspeed (m/s)               ', &
            '        0.0   HWindSpeed     - Horizontal windspeed (m/s)               ' &
        /)

        CALL InitFileInfo(data, getTestInputFloats)

    end function

    function getTestInputInArrays()

        TYPE(FileInfoType)                  :: getTestInputInArrays
        CHARACTER(1024), DIMENSION(5)       :: data = (/ &
            ! Integer Arrays
            '         90   WindVziList    - List of coordinates in the inertial Z    ', &
            '      80,90   WindVziList    - List of coordinates in the inertial Z    ', & 
            '     80, 90   WindVziList    - List of coordinates in the inertial Z    ', & 
            '1,2,3,4,5,6   WindVziList    - List of coordinates in the inertial Z    ', &
            '1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20   WindVziList        ' & 
        /)

        CALL InitFileInfo(data, getTestInputInArrays)

    end function

    function getTestInputFloatArrays()

        TYPE(FileInfoType)                  :: getTestInputFloatArrays
        CHARACTER(1024), DIMENSION(5)       :: data = (/ &
            ! Integer Arrays
            '       90.0   WindVziList    - List of coordinates in the inertial Z    ', &
            '  80.0,90.0   WindVziList    - List of coordinates in the inertial Z    ', & 
            '   80., 90.   WindVziList    - List of coordinates in the inertial Z    ', & 
            '   80.3, 90   WindVziList    - List of coordinates in the inertial Z    ', & 
            '1.1,2.2,3.3, 4.4,5.5,6.6,7.7,8.8   WindVziList    - List of coordinates ' &
        /)

        CALL InitFileInfo(data, getTestInputFloatArrays)

    end function

    function getTestInputLoArrays()

        TYPE(FileInfoType)                  :: getTestInputLoArrays
        CHARACTER(1024), DIMENSION(3)       :: data = (/ &
            ! Integer Arrays
            '       true   WindVziList    - List of coordinates in the inertial Z    ', &
            ' true,false   WindVziList    - List of coordinates in the inertial Z    ', & 
            'true, false   WindVziList    - List of coordinates in the inertial Z    ' &
        /)

        CALL InitFileInfo(data, getTestInputLoArrays)

    end function

end module
