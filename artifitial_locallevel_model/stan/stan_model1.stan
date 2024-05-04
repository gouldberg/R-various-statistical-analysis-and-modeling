// ���f���F�K��y���[�J�����x�����f���A�p�����[�^�����m�z

data{
  int<lower=1>   t_max;
  vector[t_max]   y;       // �ϑ��l

  cov_matrix[1]   W;       // ��ԎG���̕��U
  cov_matrix[1]   V;       // �ϑ��G���̕��U
  real           m0;       // ���O���z�̕���
  cov_matrix[1]  C0;       // ���O���z�̕��U
}


parameters{
  real           x0;       // ���[0]
  vector[t_max]   x;       // ���[1:t_max]
}


model{
  // �ޓx�̕���
  /* �ϑ������� */
  for (t in 1:t_max){
    y[t] ~ normal(x[t], sqrt(V[1, 1]));
  }

  // ���O���z�̕���
  /* ��Ԃ̎��O���z */
  x0   ~ normal(m0, sqrt(C0[1, 1]));

  /* ��ԕ����� */
  x[1] ~ normal(x0, sqrt(W[1, 1]));
  for (t in 2:t_max){
    x[t] ~ normal(x[t-1], sqrt(W[1, 1]));
  }
}

