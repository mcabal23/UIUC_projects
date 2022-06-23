find_dist = function(input_velo, input_spin, avg_velo, avg_spin, sd_velo, sd_spin) {
  if(sd_velo == 0 | sd_spin == 0) {
    return(NA)
  }
  adjusted_vel = (avg_velo - input_velo) / sd_velo
  adjusted_spin = (avg_spin - input_spin) / sd_spin
  return(sqrt(adjusted_vel^2 + adjusted_spin^2))
}