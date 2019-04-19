#include <random>
#include <thread>

void genTrace(double p, int N){
  double new_sam, c_mean, c_var;
  std::bernoulli_distribution d(p); // Bernoulli rv
  for (int n=1; n<=N; n++){         // generate traces
    if(d(rng)) 
      new_sam += 1.;
    else
      new_sam += -1.; 

    // calculate online variance(SS)
    c_var += double(n-1)/n * (new_sam-c_mean)*(new_sam-c_mean);
    // calculate online mean
    c_mean += (new_sam-c_mean)/n;
  }
  printf("Thread Results\n Final:%d\t Mean:%f\t Var:%f\n", 
    int(new_sam), c_mean, c_var/(N-1));
}

int main() { 

  std::cout << "Random Walk, p=0.5\n";
  std::cout << "############################\n";

  std::thread t1(genTrace, 0.5, 100);
  std::thread t2(genTrace, 0.5, 100);
  std::thread t3(genTrace, 0.5, 100);

  t1.join();t2.join();t3.join();

  std::cout << "Random Walk, p=0.55\n";
  std::cout << "############################\n";

  std::thread t4(genTrace, 0.55, 100);
  std::thread t5(genTrace, 0.55, 100);
  std::thread t6(genTrace, 0.55, 100);

  t4.join();t5.join();t6.join();

    return 0; 
}