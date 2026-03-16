import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

#1- 1、组织数据
# 预测出的比例
sa_pre = [53.77, 93.23, 97.03, 99.51]#DH4
sm_pre = [46.23, 6.77, 2.97, 0.49] #DH8
x_label = ['Mock (50:50)', 'Mock (90:10)', 'Mock (99:1)', 'Mock (999:1)']
# 实际的比例
sa_true = [50, 90, 99, 99.9]
sm_true = [50, 10, 1, 0.1]

#1- 2、画图
plt.figure(figsize=(11, 8), dpi=100)
x_num = np.array([1, 2, 3, 4])
plt.bar(x_num-0.18, sa_pre, 0.35, color='turquoise', align='center', label='DH4')  #dodgerblue
plt.bar(x_num+0.18, sa_true, 0.35, color='turquoise', align='center')
plt.bar(x_num-0.18, sm_pre, 0.35, bottom=sa_pre, color='orange', label='DH8')
plt.bar(x_num+0.18, sm_true, 0.35, bottom=sa_true, color='orange')
# 在柱子上增加标记是预测还是真实
for i in range(0,4): #for内语句为添加柱状图的数据标签，x4[i]+0.3代表数据横坐标，y1[i]+0.0003代表数据纵坐标，y1[i]数据标签内容
    plt.text(x_num[i]-0.18, 101, 'predicted', ha='center', va='bottom', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, 101, 'true', ha='center', va='bottom', fontsize=10, color='k')
for i in range(0,4): #for内语句为添加柱状图的数据标签，x4[i]+0.3代表数据横坐标，y1[i]+0.0003代表数据纵坐标，y1[i]数据标签内容
    plt.text(x_num[i]-0.18, sa_pre[i]/2, round(sa_pre[i],2), ha='center', va='center', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, sa_true[i]/2, round(sa_true[i],2), ha='center', va='center', fontsize=10, color='k')
    plt.text(x_num[i]-0.18, 100-sm_pre[i]/2, round(sm_pre[i],2), ha='center', va='center', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, 100-sm_true[i]/2, round(sm_true[i],2), ha='center', va='center', fontsize=10, color='k')
plt.xticks(x_num,x_label)
plt.legend(bbox_to_anchor=(1.005, 0.90), loc=3, borderaxespad=1)  # 图例在图形外面
plt.show()

#2- 1、组织数据
# 预测出的比例
sa_pre = [53.32, 50, 47.31] #DH4
sm_pre = [46.68, 50, 52.69] #DH8
x_label = ['Parallel-1 (50:50)', 'Parallel-2 (50:50)', 'Parallel-3 (50:50)']
# 实际的比例
sa_true = [50, 50, 50]
sm_true = [50, 50, 50]

#2- 2、画图
plt.figure(figsize=(11, 8), dpi=100)
x_num = np.array([1, 2, 3])
plt.bar(x_num-0.18, sa_pre, 0.35, color='dodgerblue', align='center', label='K56')
plt.bar(x_num+0.18, sa_true, 0.35, color='dodgerblue', align='center')
plt.bar(x_num-0.18, sm_pre, 0.35, bottom=sa_pre, color='orange', label='ET22')
plt.bar(x_num+0.18, sm_true, 0.35, bottom=sa_true, color='orange')
# 在柱子上增加标记是预测还是真实
for i in range(0,3): #for内语句为添加柱状图的数据标签，x4[i]+0.3代表数据横坐标，y1[i]+0.0003代表数据纵坐标，y1[i]数据标签内容
    plt.text(x_num[i]-0.18, 101, 'predicted', ha='center', va='bottom', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, 101, 'true', ha='center', va='bottom', fontsize=10, color='k')
for i in range(0,3): #for内语句为添加柱状图的数据标签，x4[i]+0.3代表数据横坐标，y1[i]+0.0003代表数据纵坐标，y1[i]数据标签内容
    plt.text(x_num[i]-0.18, sa_pre[i]/2, round(sa_pre[i],2), ha='center', va='center', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, sa_true[i]/2, round(sa_true[i],2), ha='center', va='center', fontsize=10, color='k')
    plt.text(x_num[i]-0.18, 100-sm_pre[i]/2, round(sm_pre[i],2), ha='center', va='center', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, 100-sm_true[i]/2, round(sm_true[i],2), ha='center', va='center', fontsize=10, color='k')
plt.xticks(x_num,x_label)
plt.legend(bbox_to_anchor=(1.005, 0.90), loc=3, borderaxespad=1)  # 图例在图形外面
plt.show()

# 3- 1、组织数据------------3-------所有平行-------------------
# 预测出的比例
sa_pre = [53.32, 50, 47.31, 8.73, 8.69, 8.58] #ET22
sm_pre = [46.68, 50, 52.69, 91.27, 91.31, 91.42] #K56
x_label = ['Parallel-1 (1:1)', 'Parallel-2 (1:1)', 'Parallel-3 (1:1)','Parallel-1 (1:10)', 'Parallel-2 (1:10)', 'Parallel-3 (1:10)']
# 实际的比例
sa_true = [50, 50, 50, 9.09, 9.09, 9.09]
sm_true = [50, 50, 50, 90.91, 90.91, 90.91]

# 3-2、画图
plt.figure(figsize=(11, 8), dpi=100)
x_num = np.array([1, 2, 3,4,5,6])
plt.bar(x_num-0.18, sa_pre, 0.35, color='dodgerblue', align='center', label='ET22')
plt.bar(x_num+0.18, sa_true, 0.35, color='dodgerblue', align='center')
plt.bar(x_num-0.18, sm_pre, 0.35, bottom=sa_pre, color='orange', label='K56')
plt.bar(x_num+0.18, sm_true, 0.35, bottom=sa_true, color='orange')
# 在柱子上增加标记是预测还是真实
for i in range(0,6): #for内语句为添加柱状图的数据标签，x4[i]+0.3代表数据横坐标，y1[i]+0.0003代表数据纵坐标，y1[i]数据标签内容
    plt.text(x_num[i]-0.18, 101, 'predicted', ha='center', va='bottom', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, 101, 'true', ha='center', va='bottom', fontsize=10, color='k')
for i in range(0,6): #for内语句为添加柱状图的数据标签，x4[i]+0.3代表数据横坐标，y1[i]+0.0003代表数据纵坐标，y1[i]数据标签内容
    plt.text(x_num[i]-0.18, sa_pre[i]/2, round(sa_pre[i],2), ha='center', va='center', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, sa_true[i]/2, round(sa_true[i],2), ha='center', va='center', fontsize=10, color='k')
    plt.text(x_num[i]-0.18, 100-sm_pre[i]/2, round(sm_pre[i],2), ha='center', va='center', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, 100-sm_true[i]/2, round(sm_true[i],2), ha='center', va='center', fontsize=10, color='k')
plt.xticks(x_num,x_label)
plt.legend(bbox_to_anchor=(1.005, 0.90), loc=3, borderaxespad=1)  # 图例在图形外面
plt.show()

# 4-1、组织数据-----------------4-------单个平均--------------
# 预测出的比例
sa_pre = [50.21, 8.67] #ET22
sm_pre = [49.79, 91.33] #K56
x_label = ['ET22:K56 (1:1)', 'ET22:K56 (1:10)']
# 实际的比例
sa_true = [50, 9.09]
sm_true = [50, 90.91]

# 4-2、画图
plt.figure(figsize=(11, 8), dpi=100)
x_num = np.array([1, 2])
plt.bar(x_num-0.18, sa_pre, 0.35, color='turquoise', align='center', label='ET22')
plt.bar(x_num+0.18, sa_true, 0.35, color='turquoise', align='center')
plt.bar(x_num-0.18, sm_pre, 0.35, bottom=sa_pre, color='orange', label='K56')
plt.bar(x_num+0.18, sm_true, 0.35, bottom=sa_true, color='orange')
# 在柱子上增加标记是预测还是真实
for i in range(0,2): #for内语句为添加柱状图的数据标签，x4[i]+0.3代表数据横坐标，y1[i]+0.0003代表数据纵坐标，y1[i]数据标签内容
    plt.text(x_num[i]-0.18, 101, 'predicted', ha='center', va='bottom', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, 101, 'true', ha='center', va='bottom', fontsize=10, color='k')
for i in range(0,2): #for内语句为添加柱状图的数据标签，x4[i]+0.3代表数据横坐标，y1[i]+0.0003代表数据纵坐标，y1[i]数据标签内容
    plt.text(x_num[i]-0.18, sa_pre[i]/2, round(sa_pre[i],2), ha='center', va='center', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, sa_true[i]/2, round(sa_true[i],2), ha='center', va='center', fontsize=10, color='k')
    plt.text(x_num[i]-0.18, 100-sm_pre[i]/2, round(sm_pre[i],2), ha='center', va='center', fontsize=10, color = 'k')
    plt.text(x_num[i]+0.18, 100-sm_true[i]/2, round(sm_true[i],2), ha='center', va='center', fontsize=10, color='k')
plt.xticks(x_num,x_label)
plt.legend(bbox_to_anchor=(1.005, 0.90), loc=3, borderaxespad=1)  # 图例在图形外面
plt.show()

# 5-1、组织数据------------3-------PHA--Fig6C-------------------
# 预测出的比例
sa_pre = [90.04, 99.53, 99.01, 100, 100, 98.90] #P3HB
sm_pre = [9.96, 0.47, 0.99, 0, 0, 1.10] #P34HB
x_label = ['18 ', '20 ', '22 ','24', '26', '28']
# 实际的比例
#sa_true = [50, 50, 50, 9.09, 9.09, 9.09]
#sm_true = [50, 50, 50, 90.91, 90.91, 90.91]

# 5-2、画图
plt.figure(figsize=(12, 8), dpi=100)
x_num = np.array([1, 2, 3,4,5,6])
plt.bar(x_num, sa_pre, 0.45, color='dodgerblue', align='center', label='P3HB')
#plt.bar(x_num+0.18, sa_true, 0.35, color='dodgerblue', align='center')
plt.bar(x_num, sm_pre, 0.45, bottom=sa_pre, color='orange', label='P34HB')
#plt.bar(x_num+0.18, sm_true, 0.35, bottom=sa_true, color='orange')
# 在柱子上增加标记是预测还是真实
#for i in range(0,6): #for内语句为添加柱状图的数据标签，x4[i]+0.3代表数据横坐标，y1[i]+0.0003代表数据纵坐标，y1[i]数据标签内容
   # plt.text(x_num[i]-0.18, 101, 'predicted', ha='center', va='bottom', fontsize=10, color = 'k')
   # plt.text(x_num[i]+0.18, 101, 'true', ha='center', va='bottom', fontsize=10, color='k')
for i in range(0,6): #for内语句为添加柱状图的数据标签，x4[i]+0.3代表数据横坐标，y1[i]+0.0003代表数据纵坐标，y1[i]数据标签内容
    plt.text(x_num[i], sa_pre[i]/2, round(sa_pre[i],2), ha='center', va='center', fontsize=15, color = 'k')
  #  plt.text(x_num[i]+0.18, sa_true[i]/2, round(sa_true[i],2), ha='center', va='center', fontsize=10, color='k')
    plt.text(x_num[i], 100-sm_pre[i]/2, round(sm_pre[i],2), ha='center', va='center', fontsize=15, color = 'k')
  #  plt.text(x_num[i]+0.18, 100-sm_true[i]/2, round(sm_true[i],2), ha='center', va='center', fontsize=10, color='k')
plt.xticks(x_num,x_label)
plt.legend(bbox_to_anchor=(1.005, 0.90), loc=3, borderaxespad=1)  # 图例在图形外面
plt.xlabel('Time (h)', fontsize=15)  # 横坐标标签
plt.ylabel('Percentage of predictions (%)', fontsize=15)  # 纵坐标标签
# 设置刻度线标签的字体大小
plt.xticks(fontsize=15)  # 横坐标刻度标签字体大小
plt.yticks(fontsize=15)  # 纵坐标刻度标签字体大小

# 显示图例并设置字体大小
plt.legend(fontsize=15)
# 保存图形为 PDF 格式
# plt.savefig('sine_wave_example.pdf', format='pdf')
plt.show()
