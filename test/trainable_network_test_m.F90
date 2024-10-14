! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module trainable_network_test_m
  !! Define inference tests and procedures required for reporting results

  ! External dependencies
  use assert_m, only : assert, intrinsic_array_t
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring, string_t, bin_t, &
    vector_test_description_t, vector_function_strategy_t
#ifdef __GFORTRAN__
  use julienne_m, only : test_function_i
#endif

  ! Internal dependencies
  use inference_engine_m, only : trainable_network_t, inference_engine_t, tensor_t, input_output_pair_t, mini_batch_t, shuffle
  implicit none

  private
  public :: trainable_network_test_t

  type, extends(vector_function_strategy_t) :: and_gate_test_function_t
  contains
    procedure, nopass :: vector_function => and_gate_with_skewed_training_data
  end type
    
  type, extends(vector_function_strategy_t) :: not_and_test_function_t
  contains
    procedure, nopass :: vector_function => not_and_gate_with_skewed_training_data
  end type
    
  type, extends(vector_function_strategy_t) :: or_gate_test_function_t
  contains
    procedure, nopass :: vector_function => or_gate_with_random_weights
  end type
    
  type, extends(vector_function_strategy_t) :: xor_gate_test_function_t
  contains
    procedure, nopass :: vector_function => xor_gate_with_random_weights
  end type
    
  type, extends(test_t) :: trainable_network_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  real, parameter :: false = 0., true = 1.

  abstract interface

    function map_i(inputs) result(expected_outputs)
      import tensor_t
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) expected_outputs
    end function

  end interface

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A trainable_network_t object"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:), vector_test_results(:)
    type(test_description_t), allocatable :: scalar_test_descriptions(:)
    type(vector_test_description_t), allocatable :: vector_test_descriptions(:)
    type(xor_gate_test_function_t) xor_gate_test_function
    type(or_gate_test_function_t) or_gate_test_function
    type(not_and_test_function_t) not_and_test_function
    type(and_gate_test_function_t) and_gate_test_function

#ifndef __GFORTRAN__
    scalar_test_descriptions = [ &
      test_description_t("preserving an identity mapping with 2 hidden layers", &
         preserves_identity_mapping), &
      test_description_t("training a perturbed identity mapping to converge to an identity mapping using the Adam optimizer", &
         perturbed_identity_converges) &
      ]
#else
    procedure(test_function_i), pointer :: preserves_identity_ptr, perturbed_identity_ptr
    preserves_identity_ptr => preserves_identity_mapping
    perturbed_identity_ptr => perturbed_identity_converges

    scalar_test_descriptions = [ &
      test_description_t( &
         "preserving an identity mapping with 2 hidden layers", &
         preserves_identity_ptr), &
      test_description_t("training a perturbed identity mapping to converge to an identity mapping using the Adam optimizer", &
         perturbed_identity_ptr) &
      ]
#endif

    vector_test_descriptions = [ &
      vector_test_description_t( &
        [string_t("learning to map (true,true)->true with 2 hidden layers trained on skewed AND-gate data") &
        ,string_t("learning to map (false,true)->false with 2 hidden layers trained on skewed AND-gate data") &
        ,string_t("learning to map (true,false)->false with 2 hidden layers trained on skewed AND-gate data") &
        ,string_t("learning to map (false,false)->false with 2 hidden layers trained on skewed AND-gate data") &
        ], and_gate_test_function), &
      vector_test_description_t( &
        [string_t("learning to map (true,true)->false with 2 hidden layers trained on skewed NOT-AND-gate data") &
        ,string_t("learning to map (false,true)->true with 2 hidden layers trained on skewed NOT-AND-gate data") &
        ,string_t("learning to map (true,false)->true with 2 hidden layers trained on skewed NOT-AND-gate data") &
        ,string_t("learning to map (false,false)->true with 2 hidden layers trained on skewed NOT-AND-gate data") &
        ], not_and_test_function), &
      vector_test_description_t( &
        [string_t("learning to map (true,true)->true with 2 hidden layers trained on symmetric OR-gate data & random weights") &
        ,string_t("learning to map (false,true)->true with 2 hidden layers trained on symmetric OR-gate data & random weights") &
        ,string_t("learning to map (true,false)->true with 2 hidden layers trained on symmetric OR-gate data & random weights") &
        ,string_t("learning to map (false,false)->false with 2 hidden layers trained on symmetric OR-gate data & random weights") &
        ], or_gate_test_function), &
      vector_test_description_t( &
      [string_t("learning (true,true)->false with 2 hidden layers trained on symmetric XOR-gate data & random weights with Adam")&
      ,string_t("learning (false,true)->true with 2 hidden layers trained on symmetric XOR-gate data & random weights with Adam")&
      ,string_t("learning (true,false)->true with 2 hidden layers trained on symmetric XOR-gate data & random weights with Adam")&
      ,string_t("learning (false,false)->false with 2 hidden layers trained on symmetric XOR-gate data & random weights with Adam")&
        ], xor_gate_test_function) &
      ]

    associate( &
      substring_in_subject => index(subject(), test_description_substring) /= 0, &
      substring_in_description => scalar_test_descriptions%contains_text(string_t(test_description_substring)), &
      num_vector_tests => size(vector_test_descriptions) &
    )
      scalar_test_descriptions = pack(scalar_test_descriptions, substring_in_subject .or. substring_in_description)

      block
        integer i

        associate( &
          substring_in_description_vector => &
            [(any(vector_test_descriptions(i)%contains_text(test_description_substring)), i=1,num_vector_tests)] &
        )
          if (substring_in_subject) then
            vector_test_results = [(vector_test_descriptions(i)%run(), i=1,num_vector_tests)]
          else if (any(substring_in_description_vector)) then
              vector_test_descriptions = pack(vector_test_descriptions, substring_in_description_vector)
              vector_test_results =  [(vector_test_descriptions(i)%run(), i=1,size(vector_test_descriptions))]
              vector_test_results =  &
                pack(vector_test_results, vector_test_results%description_contains(string_t(test_description_substring)))
           else
            vector_test_results = [test_result_t::]
          end if
          test_results = [scalar_test_descriptions%run(), vector_test_results]
        end associate
      end block
    end associate

  end function

  subroutine print_truth_table(gate_name, gate_function_ptr, test_inputs, actual_outputs)
    !! Usage: 
    !!   procedure(map_i), pointer :: xor_ptr
    !!   xor_ptr => xor
    !!   call print_truth_table("XOR", xor_ptr, test_inputs, actual_outputs)
    character(len=*), intent(in) :: gate_name
    procedure(map_i), intent(in), pointer :: gate_function_ptr
    type(tensor_t), intent(in), dimension(:) :: test_inputs, actual_outputs
    type(tensor_t) expected_outputs
    integer i

    call assert( size(test_inputs) == size(actual_outputs), &
      "trainable_network_test_m(print_truth_table): size(test_inputs) == size(actual_outputs)")

    print *,"_______" // gate_name // "_______"

    do i = 1, size(test_inputs)
      expected_outputs = gate_function_ptr(test_inputs(i))
      print *,test_inputs(i)%values(), "-->", expected_outputs%values(), ":", actual_outputs(i)%values()
    end do
  end subroutine

  function two_zeroed_hidden_layers() result(trainable_network)
    type(trainable_network_t) trainable_network
    integer, parameter :: inputs = 2, outputs = 1, hidden = 3 ! number of neurons in input, output, and hidden layers
    integer, parameter :: neurons(*) = [inputs, hidden, hidden, outputs] ! neurons per layer
    integer, parameter :: max_neurons = maxval(neurons), layers=size(neurons) ! max layer width, number of layers
    real w(max_neurons, max_neurons, layers-1), b(max_neurons, max_neurons)

    w = 0.
    b = 0.

    trainable_network = trainable_network_t( inference_engine_t( &
      nodes = neurons, weights = w, biases = b &
     ,metadata = [string_t("2-hide|3-wide"), string_t("Rouson"), string_t("2023-06-30"), string_t("sigmoid"), string_t("false")] &
    ))
  end function

  function two_random_hidden_layers() result(trainable_network)
    type(trainable_network_t) trainable_network
    integer, parameter :: inputs = 2, outputs = 1, hidden = 3 ! number of neurons in input, output, and hidden layers
    integer, parameter :: neurons(*) = [inputs, hidden, hidden, outputs] ! neurons per layer
    integer, parameter :: max_neurons = maxval(neurons), layers=size(neurons) ! max layer width, number of layers
    real w(max_neurons, max_neurons, layers-1), b(max_neurons, max_neurons)

    call random_number(b)
    call random_number(w)
    b = 2*b
    w = 2*w

    trainable_network = trainable_network_t( inference_engine_t( &
      nodes = neurons, weights = w, biases = b &
      ,metadata = [string_t("2-hide|3-wide"), string_t("Rouson"), string_t("2023-06-30"), string_t("sigmoid"), string_t("false")] &
    ))
  end function

  function and_gate_with_skewed_training_data() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(tensor_t), allocatable, dimension(:,:) :: training_inputs, training_outputs
    type(tensor_t), allocatable, dimension(:) :: tmp, tmp2, test_inputs, expected_test_outputs, actual_outputs
    type(trainable_network_t) trainable_network
    real, parameter :: tolerance = 1.E-02
    real, allocatable :: harvest(:,:,:)
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=20000
    integer batch, iter, i

    allocate(harvest(num_inputs, mini_batch_size, num_iterations))
    call random_number(harvest)
    harvest = 2.*(harvest - 0.5) ! skew toward more input values being true

    ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
    ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
    tmp = [([(tensor_t(merge(true, false, harvest(:,batch,iter) < 0.5E0)), batch=1, mini_batch_size)], iter=1, num_iterations)]
    training_inputs = reshape(tmp, [mini_batch_size, num_iterations])

    tmp2 = [([(and(training_inputs(batch, iter)), batch = 1, mini_batch_size)], iter = 1, num_iterations )]
    training_outputs = reshape(tmp2, [mini_batch_size, num_iterations])

    mini_batches = [(mini_batch_t(input_output_pair_t(training_inputs(:,iter), training_outputs(:,iter))), iter=1, num_iterations)]        
    trainable_network = two_zeroed_hidden_layers()

    call trainable_network%train(mini_batches, adam=.false., learning_rate=1.5)

    test_inputs = [tensor_t([true,true]), tensor_t([false,true]), tensor_t([true,false]), tensor_t([false,false])]
    expected_test_outputs = [(and(test_inputs(i)), i=1, size(test_inputs))]
    actual_outputs = trainable_network%infer(test_inputs)
    test_passes = [(abs(actual_outputs(i)%values() - expected_test_outputs(i)%values()) < tolerance, i=1, size(actual_outputs))]

  contains

    elemental function and(inputs_object) result(expected_outputs_object)
      type(tensor_t), intent(in) :: inputs_object 
      type(tensor_t) expected_outputs_object 
      expected_outputs_object = tensor_t([merge(true, false, sum(inputs_object%values()) > 1.99)])
    end function

  end function

  function not_and_gate_with_skewed_training_data() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(tensor_t), allocatable :: training_inputs(:,:), tmp(:), test_inputs(:)
    type(tensor_t), allocatable :: training_outputs(:,:), expected_test_outputs(:), tmp2(:)
    type(trainable_network_t) trainable_network
    type(tensor_t), allocatable :: actual_outputs(:)
    real, parameter :: tolerance = 1.E-02
    real, allocatable :: harvest(:,:,:)
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=30000
    integer batch, iter, i

    allocate(harvest(num_inputs, mini_batch_size, num_iterations))
    call random_number(harvest)
    harvest = 2.*(harvest - 0.5) ! skew toward more input values being true

    ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
    ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
    tmp = [([(tensor_t(merge(true, false, harvest(:,batch,iter) < 0.5E0)), batch=1, mini_batch_size)], iter=1, num_iterations)]
    training_inputs = reshape(tmp, [mini_batch_size, num_iterations])

    tmp2 = [([(not_and(training_inputs(batch, iter)), batch = 1, mini_batch_size)], iter = 1, num_iterations )]
    training_outputs = reshape(tmp2, [mini_batch_size, num_iterations])

    mini_batches = [(mini_batch_t(input_output_pair_t(training_inputs(:,iter), training_outputs(:,iter))), iter=1, num_iterations)]        
    trainable_network = two_zeroed_hidden_layers()

    call trainable_network%train(mini_batches, adam=.false., learning_rate=1.5)

    test_inputs = [tensor_t([true,true]), tensor_t([false,true]), tensor_t([true,false]), tensor_t([false,false])]
    expected_test_outputs = [(not_and(test_inputs(i)), i=1, size(test_inputs))]
    actual_outputs = trainable_network%infer(test_inputs)
    test_passes = [(abs(actual_outputs(i)%values() - expected_test_outputs(i)%values()) < tolerance, i=1, size(actual_outputs))]

  contains
    
    function not_and(inputs) result(expected_outputs)
       type(tensor_t), intent(in) :: inputs
       type(tensor_t) expected_outputs
       expected_outputs = tensor_t([merge(true, false, sum(inputs%values()) < 2.)])
    end function

  end function

  function or_gate_with_random_weights() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(tensor_t), allocatable :: training_inputs(:,:), test_inputs(:), actual_outputs(:)
    type(tensor_t), allocatable :: training_outputs(:,:), expected_test_outputs(:)
    type(trainable_network_t) trainable_network
    real, parameter :: tolerance = 1.E-02
    real, allocatable :: harvest(:,:,:)
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=50000
    integer batch, iter, i

    allocate(harvest(num_inputs, mini_batch_size, num_iterations))
    call random_number(harvest)

    allocate(training_inputs(mini_batch_size, num_iterations))
    allocate(training_outputs(mini_batch_size, num_iterations))
    do concurrent(batch=1:mini_batch_size, iter=1:num_iterations)
      training_inputs(batch, iter) = tensor_t(merge(true, false, harvest(:,batch,iter) < 0.5E0))
      training_outputs(batch, iter) = or(training_inputs(batch, iter))
    end do

    allocate(mini_batches(size(training_inputs,1)*num_iterations))
    do concurrent(iter=1:num_iterations)
      mini_batches(iter) = mini_batch_t(input_output_pair_t(training_inputs(:,iter), training_outputs(:,iter)))
    end do

    trainable_network = two_random_hidden_layers()

    call trainable_network%train(mini_batches, adam=.false., learning_rate=1.5)

    test_inputs = [tensor_t([true,true]), tensor_t([false,true]), tensor_t([true,false]), tensor_t([false,false])]
    expected_test_outputs = [(or(test_inputs(i)), i=1, size(test_inputs))]
    actual_outputs = trainable_network%infer(test_inputs)
    test_passes = [(abs(actual_outputs(i)%values() - expected_test_outputs(i)%values()) < tolerance, i=1, size(actual_outputs))]

  contains
    
    pure function or(inputs) result(expected_outputs)
       type(tensor_t), intent(in) :: inputs
       type(tensor_t) expected_outputs
       expected_outputs = tensor_t([merge(true, false, sum(inputs%values()) > 0.99)])
    end function

  end function

  function xor_gate_with_random_weights() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(tensor_t), allocatable, dimension(:,:) :: training_inputs, training_outputs 
    type(tensor_t), allocatable, dimension(:) :: actual_outputs, test_inputs, expected_test_outputs
    type(trainable_network_t) trainable_network
    real, parameter :: tolerance = 1.E-02
    real, allocatable :: harvest(:,:,:)
#ifdef __flang__
      !! Reducing num_iterations yields a less robust test, but moving away from local minima by
      !! increasing num_iterations causes this test to crash when compiled with the flang or ifx compilers.
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=50000
#elif defined __INTEL_COMPILER
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=49000
#else
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=500000
      !! Depending on where in the random-number sequence the weights start, this test can pass for lower
      !! numbers of iterations, e.g., 400000. Using more iterations gives more robust convergence.
#endif
    integer batch, iter

    allocate(harvest(num_inputs, mini_batch_size, num_iterations))
    call random_number(harvest)

    allocate(training_inputs(mini_batch_size, num_iterations))
    allocate(training_outputs(mini_batch_size, num_iterations))
    do concurrent(batch=1:mini_batch_size, iter=1:num_iterations)
      training_inputs(batch, iter) = tensor_t(merge(true, false, harvest(:,batch,iter) < 0.5E0))
      training_outputs(batch, iter) = local_xor(training_inputs(batch, iter))
    end do

    allocate(mini_batches(size(training_inputs,1)*num_iterations))
    do concurrent(iter=1:num_iterations)
      mini_batches(iter) = mini_batch_t(input_output_pair_t(training_inputs(:,iter), training_outputs(:,iter)))
    end do

    trainable_network = two_random_hidden_layers()

    call trainable_network%train(mini_batches, adam=.true., learning_rate=1.5)

    test_inputs = [tensor_t([true,true]), tensor_t([false,true]), tensor_t([true,false]), tensor_t([false,false])]
    block
      integer i

      expected_test_outputs = [(local_xor(test_inputs(i)), i=1, size(test_inputs))]
      actual_outputs = trainable_network%infer(test_inputs)
      test_passes = [(abs(actual_outputs(i)%values() - expected_test_outputs(i)%values()) < tolerance, i=1, size(actual_outputs))]
    end block

  contains
    
    pure function local_xor(inputs) result(expected_outputs)
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) expected_outputs
      associate(sum_inputs => sum(inputs%values()))
       expected_outputs = tensor_t([merge(true, false, sum_inputs > 0.99 .and. sum_inputs < 1.01)])
      end associate
    end function

  end function

  function perturbed_identity_network(perturbation_magnitude) result(trainable_network)
    type(trainable_network_t) trainable_network
    real, intent(in) :: perturbation_magnitude
    integer, parameter :: nodes_per_layer(*) = [2, 2, 2, 2]
    integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)
#ifndef _CRAYFTN
    real, parameter :: identity(*,*,*) = &
      reshape([real:: [1,0], [0,1] ,[1,0], [0,1], [1,0], [0,1]], [max_n, max_n, layers-1])
#else
    real, allocatable :: identity(:,:,:)
#endif
    real harvest(size(identity,1), size(identity,2), size(identity,3))

#ifdef _CRAYFTN
    identity = reshape([real:: [1,0], [0,1] ,[1,0], [0,1], [1,0], [0,1]], [max_n, max_n, layers-1])
#endif

    call random_number(harvest)
    harvest = perturbation_magnitude*harvest

    trainable_network = trainable_network_t( inference_engine_t( &
      nodes = nodes_per_layer, &
      weights = identity + harvest , & 
      biases = reshape([real:: [0,0], [0,0], [0,0]], [max_n, layers-1]), &
      metadata = [string_t("Identity"), string_t("Damian Rouson"), string_t("2023-09-18"), string_t("relu"), string_t("false")] &
    ))
  end function

  function preserves_identity_mapping() result(test_passes)
    logical test_passes
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable :: inputs(:)
    type(trainable_network_t)  trainable_network
    type(bin_t), allocatable :: bins(:)
    real, allocatable :: cost(:)
    integer, parameter :: num_pairs = 100, num_epochs = 100, n_bins = 3
    integer i, bin, epoch

    trainable_network = perturbed_identity_network(perturbation_magnitude=0.)

    associate(num_inputs => trainable_network%num_inputs(), num_outputs => trainable_network%num_outputs())

      call assert(num_inputs == num_outputs,"trainable_network_test_m(identity_mapping): # inputs == # outputs", &
        intrinsic_array_t([num_inputs, num_outputs]) &
      )
#ifdef _CRAYFTN
      allocate(inputs(num_pairs))
      do i = 1, num_pairs
         inputs(i) = tensor_t(real([i,2*i])/num_pairs)
      end do
#else
      inputs = [(tensor_t(real([i,2*i])/num_pairs), i = 1, num_pairs)]
#endif
      associate(outputs => inputs)
        input_output_pairs = input_output_pair_t(inputs, outputs)
      end associate
      bins = [(bin_t(num_items=num_pairs, num_bins=n_bins, bin_number=bin), bin = 1, n_bins)]

      do epoch = 1,num_epochs
        mini_batches = [(mini_batch_t(input_output_pairs(bins(bin)%first():bins(bin)%last())), bin = 1, size(bins))]
        call trainable_network%train(mini_batches, cost, adam=.false., learning_rate=1.5)
      end do

      block
        real, parameter :: tolerance = 1.E-06
#if defined _CRAYFTN || __GFORTRAN__
        type(tensor_t), allocatable :: network_outputs(:)
        network_outputs = trainable_network%infer(inputs)
#else
        associate(network_outputs => trainable_network%infer(inputs))
#endif
          test_passes = maxval(abs([(network_outputs(i)%values() - inputs(i)%values(), i=1,num_pairs)])) < tolerance
#if defined _CRAYFTN || __GFORTRAN__
#else
        end associate
#endif
      end block

   end associate

  end function

  function perturbed_identity_converges() result(test_passes)
    ! test that a network that represents a randomly perturbed identity mapping converges to an identity,
    ! (i.e., mapping inputs to outputs identically). This test operates at the edge of a radius of
    ! non-convergence, i.e., for the given size training data set, decrementing num_epochs or num_bins
    ! or negating adam or not shuffling doesn't converge within the specified output-value tolerance.
    logical test_passes
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable :: inputs(:)
    type(trainable_network_t)  trainable_network
    type(bin_t), allocatable :: bins(:)
    real, allocatable :: cost(:)
    integer, parameter :: num_pairs = 6
    integer, parameter :: num_epochs = 180
    integer, parameter :: num_bins = 5 
    integer i, bin, epoch

    trainable_network = perturbed_identity_network(perturbation_magnitude=0.1)

    associate(num_inputs => trainable_network%num_inputs(), num_outputs => trainable_network%num_outputs())

      call assert(num_inputs == num_outputs,"trainable_network_test_m(identity_mapping): # inputs == # outputs", &
        intrinsic_array_t([num_inputs, num_outputs]) &
      )
#ifdef _CRAYFTN
      allocate(inputs(num_pairs))
      do i = 1, num_pairs
        inputs(i) = tensor_t(real([i,2*i])/(2*num_pairs))
      end do
#else
      inputs = [(tensor_t(real([i,2*i])/(2*num_pairs)), i = 1, num_pairs)]
#endif
      associate(outputs => inputs)
        input_output_pairs = input_output_pair_t(inputs, outputs)
      end associate
      bins = [(bin_t(num_items=num_pairs, num_bins=num_bins, bin_number=bin), bin = 1, num_bins)]

      do epoch = 1,num_epochs
        call shuffle(input_output_pairs)
        mini_batches = [(mini_batch_t(input_output_pairs(bins(bin)%first():bins(bin)%last())), bin = 1, size(bins))]
        call trainable_network%train(mini_batches, cost, adam=.true., learning_rate=1.5)
      end do

      block
        real, parameter :: tolerance = 1.E-06
#if defined _CRAYFTN || __GFORTRAN__
        type(tensor_t), allocatable :: network_outputs(:)
        network_outputs = trainable_network%infer(inputs)
#else
        associate(network_outputs => trainable_network%infer(inputs))
#endif
          test_passes = maxval(abs([(network_outputs(i)%values() - inputs(i)%values(), i=1,num_pairs)])) < tolerance
#if defined _CRAYFTN || __GFORTRAN__
#else
        end associate
#endif
      end block

   end associate

  end function

end module trainable_network_test_m
