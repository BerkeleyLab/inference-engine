submodule(trainable_network_m) trainable_network_s
  implicit none 

contains

  module procedure default_real_network
    trainable_network%inference_engine_t = inference_engine
    trainable_network%workspace_ = workspace_t(inference_engine)
  end procedure

  module procedure default_real_train
    call self%inference_engine_t%default_real_learn(mini_batches_arr, cost, adam, learning_rate, self%workspace_)
  end procedure

end submodule trainable_network_s
