test_that("UCBPolicy initialization", {
  # TODO : test with non valid parameters
  # popo <- UCBPolicy$new(10,3)
  # expect_equal(popo$alpha, 10)
  # expect_equal(popo$K,3)
  # expect_identical(popo$mu,rep(Inf,3))
  # expect_identical(popo$N,rep(0,3))
})

test_that("UCBPolicy update", {
  # popo <- UCBPolicy$new(10, 3)
  # popo$update_with_reward(10, 3)
  # expect_equal(popo$mu[3], 10)
  # popo$update_with_reward(10,3)
  # expect_equal(popo$mu[3], 10)
  # popo$update_with_reward(40,3)
  # expect_equal(popo$mu[3], 20)
})

test_that("UCBPolicy get action", {
  # popo <- UCBPolicy$new(10,3)
  # popo$update_with_reward(42,1)$update_with_reward(69,2)
  # expect_equal(popo$get_action(3),3)
  # popo$update_with_reward(420,3)
  # expect_equal(popo$get_action(4),3)
})

test_that("LINUCBPolicy initialization", {
  # TODO : test with non valid parameters
  # popo <- LINUCBPolicy$new(delta=0.5, dim=5, lambda=4, K = 3)
  # expect_equal(popo$delta,0.5)
  # expect_equal(popo$dim,5)
  # expect_equal(popo$lambda,4)
  # expect_equal(popo$K,3)
  # expect_identical(popo$b, matrix(0,5))
  # expect_identical(popo$V, 4 * diag(5))
  # expect_identical(popo$V_inv %*% popo$V, diag(5) )
  # expect_identical(popo$th_hat, matrix(0,5))
})

test_that("ThompsonSamplingPolicy initialization", {
  # TODO : test with non valid parameters
  # popo <- ThompsonSamplingPolicy$new(5)
  # expect_identical(popo$alpha, rep(0,5))
  # expect_identical(popo$beta, rep(0,5))
  # expect_equal(popo$K, 5)
})

test_that("EpsilonGreedyPolicy initialization", {
  popoDefault <- EpsilonGreedyPolicy$new()
  popoCustom <- EpsilonGreedyPolicy$new(0.9)
  expect_identical(popoDefault$epsilon, 0.25)
  expect_identical(popoCustom$epsilon, 0.9)
})

