// ���f���F�K��y���[�J�����x�����f���A�p�����[�^�����m�A�J���}���t�B���^�����p�z

data{
  int<lower=1>    t_max;   // ���n��
  matrix[1, t_max]    y;   // �ϑ��l

  matrix[1, 1]    G;       // ��ԑJ�ڍs��
  matrix[1, 1]    F;       // �ϑ��s��
  vector[1]      m0;       // ���O���z�̕���
  cov_matrix[1]  C0;       // ���O���z�̕��U
}

parameters{
  cov_matrix[1]   W;       // ��ԎG���̕��U
  cov_matrix[1]   V;       // �ϑ��G���̕��U
}

model{
  // �ޓx�̕���
  /* ���`�E�K�E�X�^��ԋ�ԃ��f���̖ޓx�����߂�֐� */
  y ~ gaussian_dlm_obs(F, G, V, W, m0, C0);

  // ���O���z�̕���
  /* W, V�̎��O���z�F����񎖑O���z�i�ȗ����̃f�t�H���g�ݒ�����p�j */
}
