mkdir src bin
echo -e "# Ignore all files in build directory\n*\n# Except .gitignore file.\n!.gitignore" >> bin/.gitignore
echo -e "cmake_minimum_required(VERSION 3.12)
project(project)

set(CMAKE_CXX_STANDARD                  17)
set(CMAKE_CXX_STANDARD_REQUIRED         17)
set(CMAKE_CXX_EXTENSIONS               OFF)

# Export comiple comands for YCM
set(CMAKE_EXPORT_COMPILE_COMMANDS       ON)


# Add executable
add_executable(main main.cc)" >> src/CMakeLists.txt

echo -e "#include <iostream>

int main() {
  std::cout << \"Hello World\" << std::endl;
  return 0;
}"  >> src/main.cc
