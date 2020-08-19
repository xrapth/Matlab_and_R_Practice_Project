% Support Vector Machine


clc; close all; clear all;

% Import training dataset
train_foldername='train_images/';
train_images=dir([train_foldername '*.png']);
train_attr=load('train_attributes.dat'); 
num_files=size(train_images, 1)

% Initialize the final data structure
trainData = zeros(num_files, 256*256);


% Pre-process training data
for j = 1:num_files
    currentimage = imread([train_foldername, '/',num2str(j) ,'.png']);
    image{j} = currentimage;
    image{j} = im2double(image{j});
    image{j} = rgb2gray(image{j}); % from change the number of color channels
    image{j} = reshape(image{j}', 1, size(image{j},1)*size(image{j},2)); % change the data shape
    trainData(j,:) = image{j}; % change the data structure so that it fits the fitsvm function's requirements
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% 1. Use 'fitcsvm' function to train an SVM:

linear_std_model = fitcsvm(trainData,train_attr, 'KernelFunction', 'linear', 'Standardize', true);
poly_std_model = fitcsvm(trainData,train_attr, 'KernelFunction', 'polynomial', 'Standardize', true);
rbf_std_model = fitcsvm(trainData,train_attr, 'KernelFunction', 'rbf', 'Standardize', true);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% 2. Use test_images dataset to test your trained model:

test_foldername='test_images/';
test_attr=load('test_attributes.dat'); 
testData = zeros(20, 256*256);

% Pre-process test data
for j = 1:20
    currentimage = imread([test_foldername, '/',num2str(j) ,'.png']);
    image{j} = currentimage;
    image{j} = im2double(image{j});
    image{j} = rgb2gray(image{j});
    image{j} = reshape(image{j}', 1, size(image{j},1)*size(image{j},2));
    testData(j,:) = image{j};
     
end

% Make predictions using your trained model and the processed test data
models = {linear_std_model; poly_std_model; rbf_std_model};
names = {'linear Standardized'; 'polynomial Standardized'; 'rbf Standardized'};
labels = {0, 0, 0};
for i = 1:3    
    [labels{i},scores] = predict(models{i}, testData);
    l = loss(models{i}, testData, test_attr);
    fprintf("Loss for %s model: %f\n", names{i}, l); 
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% 3. Visualize results using a confusion matrix
confusionchart(test_attr, labels{1}, 'Title', names{1} + " model")


