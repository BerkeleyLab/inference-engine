! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(metadata_m) metadata_s
  use assert_m, only : assert
  implicit none

contains

  module procedure strings
    components = [self%modelName_, self%modelAuthor_, self%compilationDate_, self%activationFunction_, self%usingSkipConnections_]
  end procedure 
  
  module procedure activation_name
    function_name = self%activationFunction_
  end procedure

  module procedure from_components
    metadata%modelName_ = modelName
    metadata%modelAuthor_ = modelAuthor
    metadata%compilationDate_ = compilationDate
    metadata%activationFunction_ = activationFunction
    metadata%usingSkipConnections_ = usingSkipConnections
  end procedure 

  module procedure from_json
    integer l

    call assert(lines(1)%get_json_key() == "metadata", "metadata_s(from_json): metadata found")

    do l = 2, size(lines)-1
      associate(key => lines(l)%get_json_key())
        select case (key%string())
          case("modelName")
            metadata%modelName_ = lines(l)%get_json_value(key, mold=string_t(""))
          case("modelAuthor")
            metadata%modelAuthor_ = lines(l)%get_json_value(key, mold=string_t(""))
          case("compilationDate")
            metadata%compilationDate_ = lines(l)%get_json_value(key, mold=string_t(""))
          case("activationFunction")
            metadata%activationFunction_ = lines(l)%get_json_value(key, mold=string_t(""))
          case("usingSkipConnections")
            metadata%usingSkipConnections_ = lines(l)%get_json_value(key, mold=string_t(""))
          case default
            error stop "metadata_s(from_json): missing key " // key%string()
        end select
      end associate
    end do

    call assert(any(trim(adjustl(lines(size(lines))%string())) == ["},","} "]), "metadata_s(from_json): metadata object end found")
  end procedure

  module procedure double_precision_from_json
    integer l

    call assert(lines(1)%get_json_key() == "metadata", "metadata_s(double_precision_from_json): metadata found")

    do l = 2, size(lines)-1
      associate(key => lines(l)%get_json_key())
        select case (key%string())
          case("modelName")
            metadata%modelName_ = lines(l)%get_json_value(key, mold=string_t(""))
          case("modelAuthor")
            metadata%modelAuthor_ = lines(l)%get_json_value(key, mold=string_t(""))
          case("compilationDate")
            metadata%compilationDate_ = lines(l)%get_json_value(key, mold=string_t(""))
          case("activationFunction")
            metadata%activationFunction_ = lines(l)%get_json_value(key, mold=string_t(""))
          case("usingSkipConnections")
            metadata%usingSkipConnections_ = lines(l)%get_json_value(key, mold=string_t(""))
          case default
            error stop "metadata_s(double_precision_from_json): missing key " // key%string()
        end select
      end associate
    end do

    call assert(any(trim(adjustl(lines(size(lines))%string())) == ["},","} "]), &
      "metadata_s(double_precision_from_json): metadata object end found")
  end procedure

  module procedure to_json

    character(len=*), parameter :: indent = repeat(" ",ncopies=4)

    lines = [ &
      string_t(indent // '"metadata": {'), &
      string_t(indent // indent // '"modelName" : "'  // trim(adjustl(self%modelName_%string()))  // '",' ), &
      string_t(indent // indent // '"modelAuthor" : "'  // trim(adjustl(self%modelAuthor_%string())) // '",' ), &
      string_t(indent // indent // '"compilationDate" : "' // trim(adjustl(self%compilationDate_%string())) // '",'), &
      string_t(indent // indent // '"activationFunction" : "' // trim(adjustl(self%activationFunction_%string())) // '",'), &
      string_t(indent // indent // '"usingSkipConnections" : "' // trim(adjustl(self%usingSkipConnections_%string())) // '"'), &
      string_t(indent // '}') &
    ]
  end procedure

  module procedure equals
    lhs_equals_rhs = &
      lhs%modelName_ == rhs%modelName_ .and. &
      lhs%modelAuthor_ == rhs%modelAuthor_ .and. &
      lhs%compilationDate_ == rhs%compilationDate_ .and. &
      lhs%activationFunction_ == rhs%activationFunction_ .and. &
      lhs%usingSkipConnections_ == rhs%usingSkipConnections_
  end procedure 

end submodule metadata_s
