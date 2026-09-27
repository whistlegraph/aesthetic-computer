int eg_load(const char *score);
int eg_voice(const float *samples, int frames, double at, double gain, double pan);
void eg_render(float *samples, int frames);
int eg_start(double epoch, const char *receipt);
void eg_stop(void);
double eg_time(void);
double eg_peak(void);

void eg_set_gain(double gain);
void eg_set_drive(double db);
double eg_cpu_time(void);
