#!/usr/bin/env python

import os, shutil, sys, subprocess, tarfile, tempfile
from waflib import Build, Context, Scripting, Utils

APPNAME = 'libpegged'
VERSION = '1.0'

TOP = os.curdir
OUT = 'build'

def options(opt):
    opt.load('compiler_d')

    opt.add_option('--lp64', action = 'store', default = 'true', help = 'compile for 64-bit CPUs (true/false)')
    opt.add_option('--mode', action = 'store', default = 'debug', help = 'the mode to compile in (debug/release)')
    opt.add_option('--valgrind', action = 'store', default = 'false', help = 'use Valgrind for unit tests')

def configure(conf):
    conf.load('compiler_d')

    if conf.options.valgrind != 'true' and conf.options.valgrind != 'false':
        conf.fatal('--valgrind must be either true or false.')

    conf.env.VALGRIND = conf.options.valgrind

    def add_option(option, flags = 'DFLAGS'):
        if option not in conf.env[flags]:
            conf.env.append_value(flags, option)

    if conf.env.COMPILER_D == 'dmd':
        add_option('-w')
        add_option('-wi')
        add_option('-ignore')
        add_option('-property')
        add_option('-g')

        if conf.options.mode == 'debug':
            add_option('-debug')
        elif conf.options.mode == 'release':
            add_option('-release')
            add_option('-O')
            add_option('-inline')
        else:
            conf.fatal('--mode must be either debug or release.')
    elif conf.env.COMPILER_D == 'gdc':
        add_option('-Wall')
        add_option('-fignore-unknown-pragmas')
        add_option('-fproperty')
        add_option('-g')
        add_option('-ggdb')

        if conf.options.mode == 'debug':
            add_option('-fdebug')
        elif conf.options.mode == 'release':
            add_option('-frelease')
            add_option('-O3')
        else:
            conf.fatal('--mode must be either debug or release.')
    elif conf.env.COMPILER_D == 'ldc2':
        add_option('-w')
        add_option('-wi')
        add_option('-ignore')
        add_option('-property')
        add_option('-check-printf-calls')
        add_option('-g')

        if conf.options.mode == 'debug':
            add_option('-d-debug')
        elif conf.options.mode == 'release':
            add_option('-release')
            add_option('-O3')
            add_option('--enable-inlining')
        else:
            conf.fatal('--mode must be either debug or release.')
    else:
        conf.fatal('Unsupported D compiler.')

    if conf.options.lp64 == 'true':
        add_option('-m64')
        conf.env.append_value('LINKFLAGS', '-m64')
    elif conf.options.lp64 == 'false':
        add_option('-m32')
        conf.env.append_value('LINKFLAGS', '-m32')
    else:
        conf.fatal('--lp64 must be either true or false.')

def build(bld):
    bld.stlib(source = bld.path.ant_glob([os.path.join('pegged', '*.d'),
                                          os.path.join('pegged', 'dynamic', '*.d')]),
              target = 'pegged',
              includes = [TOP])

    if bld.env.COMPILER_D == 'dmd':
        unittest = '-unittest'
    elif bld.env.COMPILER_D == 'gdc':
        unittest = '-funittest'
    elif bld.env.COMPILER_D == 'ldc2':
        unittest = '-unittest'
    else:
        bld.fatal('Unsupported D compiler.')

    bld.program(source = bld.path.ant_glob(os.path.join('pegged', 'test', '*.d')),
                target = 'pegged.tester',
                use = ['pegged'],
                includes = [TOP],
                install_path = None,
                dflags = unittest)

def _run_shell(dir, ctx, args):
    cwd = os.getcwd()
    os.chdir(dir)

    code = subprocess.Popen(args, shell = True).wait()

    if code != 0:
        ctx.fatal(str(args) + ' exited with: ' + str(code))

    os.chdir(cwd)

def unittest(ctx):
    '''runs the unit test suite'''

    if ctx.env.VALGRIND == 'true':
        cmd = 'valgrind'
        cmd += ' --suppressions=' + os.path.join(os.pardir, 'pegged.valgrind')
        cmd += ' --leak-check=full'
        cmd += ' --track-fds=yes'
        cmd += ' --num-callers=50'
        cmd += ' --show-reachable=yes'
        cmd += ' --undef-value-errors=no'
        cmd += ' --error-exitcode=1'
        cmd += ' --gen-suppressions=all'
        cmd += ' ' + os.path.join(os.curdir, 'pegged.tester')

        _run_shell(OUT, ctx, cmd)
    else:
        _run_shell(OUT, ctx, './pegged.tester')

class UnitTestContext(Build.BuildContext):
    cmd = 'unittest'
    fun = 'unittest'

def dist(dst):
    '''makes a tarball for redistributing the sources'''

    with open('.gitignore', 'r') as f:
        dst.excl = ' '.join(l.strip() for l in f if l.strip())
        dst.excl += ' .git/* .gitignore .arcconfig'

class DistCheckContext(Scripting.Dist):
    cmd = 'distcheck'
    fun = 'distcheck'

    def execute(self):
        self.recurse([os.path.dirname(Context.g_module.root_path)])
        self.archive()
        self.check()

    def check(self):
        with tarfile.open(self.get_arch_name()) as t:
            for x in t:
                t.extract(x)

        instdir = tempfile.mkdtemp('.inst', self.get_base_name())
        cfg = [x for x in sys.argv if x.startswith('-')]

        ret = Utils.subprocess.Popen([sys.argv[0],
                                      'configure',
                                      'install',
                                      'uninstall',
                                      '--destdir=' + instdir] + cfg, cwd = self.get_base_name()).wait()

        if ret:
            self.fatal('distcheck failed with code {0}'.format(ret))

        if os.path.exists(instdir):
            self.fatal('distcheck succeeded, but files were left in {0}'.format(instdir))

        shutil.rmtree(self.get_base_name())

def distcheck(ctx):
    '''checks if the project compiles (tarball from 'dist')'''

    pass

class PackageContext(Build.InstallContext):
    cmd = 'package'
    fun = 'build'

    def init_dirs(self, *k, **kw):
        super(PackageContext, self).init_dirs(*k, **kw)

        self.tmp = self.bldnode.make_node('package_tmp_dir')

        try:
            shutil.rmtree(self.tmp.abspath())
        except:
            pass
        if os.path.exists(self.tmp.abspath()):
            self.fatal('Could not remove the temporary directory {0}'.format(self.tmp))

        self.tmp.mkdir()
        self.options.destdir = self.tmp.abspath()

    def execute(self, *k, **kw):
        back = self.options.destdir

        try:
            super(PackageContext, self).execute(*k, **kw)
        finally:
            self.options.destdir = back

        files = self.tmp.ant_glob('**')

        appname = getattr(Context.g_module, Context.APPNAME, 'noname')
        version = getattr(Context.g_module, Context.VERSION, '1.0')

        ctx = Scripting.Dist()
        ctx.arch_name = '{0}-{1}-bin.tar.bz2'.format(appname, version)
        ctx.files = files
        ctx.tar_prefix = ''
        ctx.base_path = self.tmp
        ctx.archive()

        shutil.rmtree(self.tmp.abspath())

def package(ctx):
    '''packages built binaries into a tarball'''

    pass
