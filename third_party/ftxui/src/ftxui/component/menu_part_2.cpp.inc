    float animation_background_ = 0.F;
    float animation_foreground_ = 0.F;
    animation::Animator animator_background_ =
        animation::Animator(&animation_background_, 0.F);
    animation::Animator animator_foreground_ =
        animation::Animator(&animation_foreground_, 0.F);
  };

  return Make<Impl>(std::move(option));
}
