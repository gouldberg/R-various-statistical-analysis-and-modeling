data { 
  int<lower=0> N;
	real<lower=0> x[N];
}

parameters {
	real<lower=0>   mu;
	real<lower=0>	sigma;
}

model {
	for (i in 1:N)
		x[i] ~ gumbel(mu, sigma);
}

generated quantities{
	real s;          //SD
	real p_w;        //???E?L?^?͉??p?[?Z???g?_??
	real x_99;       //99?p?[?Z???g?_
	real u_rover100; //?Č????Ԃ?100?N???蒷???m??
	real r_w;        //???E?L?^?̍Č????x??(1/?Č?????)
	real xpred;      //x?̗\???l
	real p_new;      //?\???l>8.95?̊m??
	s = sqrt(pow(pi(), 2) * pow(sigma, 2)/6);
	p_w = exp(-exp((mu - 8.95) / sigma));
	x_99 = mu - sigma * (log(-log(0.99)));
	u_rover100 = 8.95 > x_99 ? 1 : 0;
	r_w = 1 / (1 - exp(-exp((mu - 8.95) / sigma)));
	xpred = gumbel_rng(mu, sigma);
	p_new = xpred > 8.95 ? 1: 0;
}


