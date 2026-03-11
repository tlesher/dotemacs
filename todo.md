# todo-benchmark.md

## Emacs Startup Benchmarking Setup

This guide outlines how to integrate `benchmark-init` into your
configuration to identify slow-loading modules.

---

### 1. Installation

Add this to your `init.el` to ensure the package is available on your
system. [95%]

```elisp
(unless (package-installed-p 'benchmark-init)
  (package-refresh-contents)
  (package-install 'benchmark-init))

```

---

### 2. Early Activation

To capture the load times of your UI and speedup files, place these
lines at the **very top** of your `early-init.el`. [98%]

```elisp
(require 'benchmark-init)
(benchmark-init/activate)

```

---

### 3. Cleanup and Results

Update your startup tasks function (in `init.el`) to stop the profiler
and display the results once Emacs is ready. [92%]

```elisp
(defun my/on-startup-tasks ()
  "Restore GC, stop benchmarking, and show results."
  ;; Restore the GC threshold
  (setq gc-cons-threshold tdl-orig-gc-cons-threshold)

  ;; Deactivate profiling
  (benchmark-init/deactivate)

  ;; Show the results in a tabulated buffer
  (benchmark-init/show-durations-tabulated)

  ;; Print the summary message
  (let ((elapsed (float-time (time-subtract after-init-time before-init-time))))
    (message "Emacs ready in %.2f seconds with %d garbage collections."
             elapsed gcs-done)))

(add-hook 'emacs-startup-hook #'my/on-startup-tasks)

```

---

### How to Analyze Results

When the benchmark buffer appears, sort by **Total Time**. Look for
outliers—packages that take significantly longer to load than
others. These are your primary candidates for lazy-loading. [90%]

Would you like me to help you set up some of those slow-loading
packages with `:defer` or `after-load` hooks once you've run the
benchmark?
