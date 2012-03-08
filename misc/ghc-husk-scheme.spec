# Copyright (c) 2012 Justin Ethier
# This file and all modifications and additions to the pristine
# package are under the same license as the package itself.
#
# Please submit bugfixes or comments to Justin Ethier via https://github.com/justinethier/husk-scheme
 
# norootforbuild
 
%define pkg_name husk-scheme
%define ghc_version %(ghc --numeric-version)

%global common_summary An R5RS-compatible Scheme written in Haskell

%global common_description An R5RS Scheme interpreter, compiler, and library written in Haskell. \
Provides advanced features including continuations, \
non-hygienic macros, a Haskell FFI, and the full numeric tower.\

#%define pkg_libdir %{_libdir}/ghc-%{ghc_version}/%{pkg_name}-%{version}
#%define pkg_docdir %{_datadir}/doc/ghc/libraries/%{pkg_name}-%{version}
 
# ghc does not emit debug information
%define debug_package %{nil}
 
Name:           ghc-%{pkg_name}
Version:        3.4
Release:        1%{?dist}
Summary:        %{common_summary}
Group:          Compilers/Interpreters, Language
License:        MIT
URL:            http://hackage.haskell.org/package/%{pkg_name}
Source0:        http://hackage.haskell.org/packages/archive/%{pkg_name}/%{version}/%{pkg_name}-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root
ExclusiveArch:  %{ghc_arches}
BuildRequires:  ghc-Cabal-devel
BuildRequires:  ghc-rpm-macros
#?? BuildRequires:  hscolour
BuildRequires:  ghc, ghc-doc, ghc-prof
BuildRequires:  ghc-parsec-devel ghc-haskeline-devel ghc-ghc-paths-devel
BuildRequires:  ghc-ghc-devel

%description
%{common_description}
 
%package devel
Summary:        Haskell %{pkg_name} library
Group:          Compilers/Interpreters, Language
Requires:       ghc = %{ghc_version}
Requires(post): ghc = %{ghc_version}
Requires(preun): ghc = %{ghc_version}
Requires:       ghc, ghc-doc, ghc-parsec-devel ghc-haskeline-devel ghc-ghc-paths-devel, ghc-ghc-devel

%description devel
%{common_description}

Provides advanced features including continuations, 
non-hygienic macros, a Haskell FFI, and the full numeric tower.
 
This package contains the development files for %{name}
built for ghc-%{ghc_version}.
 
 
%package doc
Summary:        Documentation for %{name}
Group:          Development/Libraries
Requires:       ghc-doc = %{ghc_version}
Requires(post): ghc-doc = %{ghc_version}
Requires(postun): ghc-doc = %{ghc_version}
Requires:       ghc, ghc-doc, ghc-parsec-devel ghc-haskeline-devel ghc-ghc-paths-devel, ghc-ghc-devel
 
%description doc
%{common_description}
 
This package contains development documentation files for the %{name} library.
 
 
%package prof
Summary:        Profiling libraries for %{name}
Group:          Development/Libraries
Requires:       %{name}-devel = %{version}-%{release}
Requires:       ghc-prof = %{ghc_version}
Requires:       ghc, ghc-doc, ghc-parsec-devel ghc-haskeline-devel ghc-ghc-paths-devel, ghc-ghc-devel
 
%description prof
%{common_description}
 
This package contains profiling libraries for %{name}.
 
 
%prep
%setup -q -n %{pkg_name}-%{version}
 
 
%build
%cabal_configure --ghc -p
%cabal build
%cabal haddock
%ghc_gen_scripts
 
 
%install
rm -rf $RPM_BUILD_ROOT
%cabal_install
%ghc_install_scripts
%ghc_gen_filelists %{name}
 
 
%clean
rm -rf $RPM_BUILD_ROOT
 
 
%post devel
%ghc_register_pkg
 
 
%post doc
%ghc_reindex_haddock
 
 
%preun devel
if [ "$1" -eq 0 ] ; then
  %ghc_unregister_pkg
fi
 
 
%postun doc
if [ "$1" -eq 0 ] ; then
  %ghc_reindex_haddock
fi
 
 
%files devel -f %{name}-devel.files
%defattr(-,root,root,-)
%{_docdir}/%{name}-%{version}
 
 
%files doc
%defattr(-,root,root,-)
%{pkg_docdir}
 
 
%files prof -f %{name}-prof.files
%defattr(-,root,root,-)
 
 
%changelog
