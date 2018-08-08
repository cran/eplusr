install_ep88 <- function () {
    if (is_avail_eplus(8.8)) return(invisible())

    # hard store download links for EnergyPlus 8.8 instead of using
    # `download_eplus()` and `install_eplus()` to avoid GitHub API rate limit
    ep_mac <- "https://github.com/NREL/EnergyPlus/releases/download/v8.8.0/EnergyPlus-8.8.0-7c3bbe4830-Darwin-x86_64.dmg"
    ep_linux <- "https://github.com/NREL/EnergyPlus/releases/download/v8.8.0/EnergyPlus-8.8.0-7c3bbe4830-Linux-x86_64.sh"
    ep_win32 <- "https://github.com/NREL/EnergyPlus/releases/download/v8.8.0/EnergyPlus-8.8.0-7c3bbe4830-Windows-i386.exe"
    ep_win64 <- "https://github.com/NREL/EnergyPlus/releases/download/v8.8.0/EnergyPlus-8.8.0-7c3bbe4830-Windows-x86_64.exe"

    if (os_type() != "windows") {
        dl_link <- switch(os_type(), macos = ep_mac, linux = ep_linux)
        path <- normalizePath(Sys.getenv("HOME"))
    } else {
        dl_link <- switch(os_arch(), "64bit" = ep_win64, "32bit" = ep_win32)
        path <- normalizePath(Sys.getenv("USERPROFILE"))
    }

    ori_wd <- getwd()
    on.exit(setwd(ori_wd), add = TRUE)
    setwd(path)

    f <- normalizePath(file.path(path, basename(dl_link)), mustWork = FALSE)
    if (!file.exists(f)) download_file(dl_link, f)

    # install EnergyPlus without administrator privileges
    if (os_type() == "windows") {
        system(paste0(f, "/S"))
        use_eplus(8.8)
    } else if (os_type() == "linux") {
        system(sprintf("chmod +x %s", f))
        system(sprintf("echo 'y\n%s\n' | ./%s", path, basename(f)))
        system(sprintf("chmod -R a+w %s/EnergyPlus-8-8-0", path))
        use_eplus(file.path(path, "EnergyPlus-8-8-0"))
    } else {
        no_ext <- tools::file_path_sans_ext(basename(f))
        system(sprintf("hdiutil attach %s | installer -pkg /Volumes/%s/%s.pkg -target LocalSystem",
                basename(f), no_ext, no_ext))
        use_eplus(8.8)
    }
}
