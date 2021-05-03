module radar

    use trig

    real, parameter :: ALPHA = 0.7
    real, parameter :: BETA = 0.38

    type :: vector
        real :: x
        real :: y
    contains
        generic :: operator(+) => vector_addition
        generic :: operator(-) => vector_substraction
        generic :: operator(*) => vector_times_scalar
        generic :: operator(/) => vector_divided_scalar
        procedure :: vector_addition
        procedure :: vector_substraction
        procedure :: vector_times_scalar
        procedure :: vector_divided_scalar
    end type

    type :: detection
        real :: range
        real :: azimuth ! The azimuth in compass degrees
        real :: time    ! The time of detection
        type(vector) :: cart = vector(0.0, 0.0)
    contains 
        procedure :: to_cart
        procedure :: time_diff
        procedure :: pos_diff
        procedure :: compute_velocity
    end type

    interface detection
        procedure :: new_detection

    end interface

    interface operator(*)
        procedure :: scalar_times_vector
    end interface
    

    contains 

        type(detection) function new_detection(range, azimuth, time)

            real, intent(in) :: range, azimuth, time

            new_detection%range = range
            new_detection%azimuth = azimuth
            new_detection%time = time

            new_detection%cart = new_detection%to_cart()
            print *, "new_detection called"
        
        end function


        elemental type(vector) function to_cart(detect) result(cart)        
            class(detection), intent(in) :: detect
            real :: theta

            theta = compass_to_theta(detect%azimuth)
            cart%x = detect%range * cosd(theta)
            cart%y = detect%range * sind(theta)
        end function

        elemental real function compass_to_theta(azimuth) result(theta) 
        ! Convert compass angle to standard unit circle angle
        real, intent(in) :: azimuth
        theta = azimuth * (-1) + 90

        if (theta < 0) then
            theta = theta + 360.0
        else if (theta >= 360.0) then
            theta = theat - 360.0
        end if

        end function

        elemental real function time_diff(det1, det2)
            class(detection), intent(in) :: det1, det2
            time_diff = det2%time - det1%time
        end function

        elemental type(vector) function pos_diff(det1, det2)
            class(detection), intent(in) :: det1, det2
            pos_diff = det1%cart - det2%cart
        end function

        elemental type(vector) function compute_velocity(det_t0, det_tf)
            class(detection), intent(in) :: det_t0, det_tf

            real :: delta_time
            type(vector) :: delta_pos
            delta_time = det_t0%time_diff(det_tf)
            delta_pos = det_t0%pos_diff(det_tf)

            compute_velocity = delta_pos/delta_time

        end function

        elemental type(vector) function vector_addition(v1, v2) result(v3)
            class(vector), intent(in) :: v1, v2
            v3%x = v1%x + v2%x
            v3%y = v1%y + v2%y
        end function
            
        elemental type(vector) function vector_substraction(v1, v2) result(v3)
            class(vector), intent(in) :: v1, v2
            v3%x = v1%x - v2%x
            v3%y = v1%y - v2%y
        end function

        elemental type(vector) function scalar_times_vector(scalar, vec) result(v2)
            real, intent(in) :: scalar
            class(vector), intent(in) :: vec
            v2%x = vec%x * scalar
            v2%y = vec%y * scalar
        end function

        elemental type(vector) function vector_times_scalar(vec, scalar) result(v2)
            class(vector), intent(in) :: vec
            real, intent(in) :: scalar
            v2%x = vec%x * scalar
            v2%y = vec%y * scalar
        end function

        elemental type(vector) function vector_divided_scalar(vec, scalar) result(v2)
            class(vector), intent(in) :: vec
            real, intent(in) :: scalar
            v2%x = vec%x / scalar
            v2%y = vec%y / scalar
        end function

        elemental type(vector) function predict_position(smooth_pos, prev_velocity, delta_time)

            type(vector), intent(in) :: smooth_pos, prev_velocity
            real, intent(in) :: delta_time

            predict_position = smooth_pos + (prev_velocity * delta_time)

        end function

        elemental type(vector) function update_velocity(prev_velocity, measured_pos, predicted_position, delta_time)
            type(vector), intent(in) :: prev_velocity, measured_pos, predicted_position
            real, intent(in) :: delta_time

            update_velocity = prev_velocity + (BETA/delta_time) * (measured_pos - predicted_position)
        end function

        elemental type(vector) function update_position(predicted_position, measured_position)
            type(vector), intent(in) :: predicted_position, measured_position

            update_position = predicted_position + ALPHA*(measured_position - predicted_position)
        end function

end module

