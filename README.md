# bomber
这是个小游戏，可以进行1对1对战，很有意思。

## 游戏概述

回合制游戏。炸弹人使用炸弹消灭对方炸弹人或者炸毁宝箱获取点数。
游戏为两队PK，每队固定4个炸弹人，同时上场。
游戏目标是赢取点数胜过对方。


## 地图

游戏地图由X*Y方格的空地组成，空地上可能会有墙或者箱子障碍物。
坐标原点（0，0）在地图的左上角，向右是水平方向的X坐标，向下是垂直方向的Y坐标。
地图最大为15*15。
游戏开始时炸弹人在地图中的位置由服务器决定。

## 回合制

### 1	每个回合内炸弹人可以执行的动作

 在每个回合内，每个炸弹人可以请求执行如下动作：
	使用兴奋剂
	移动前在当前位置放置炸弹
	移动
	移动成功后在当前位置放置炸弹。
以上动作在同一回合内可以一起执行。
###　2　单回合处理过程
1.	回合开始
2.	结算炸弹爆炸（相关规则见“炸弹”）
3.	广播全局最新状态
4.	收集对战双方动作指令
5.	结算所有炸弹人“使用兴奋剂”动作
6.	结算所有炸弹人“移动前在当前位置放置炸弹”动作
7.	结算所有炸弹人“移动”动作
8.	结算所有炸弹人“移动成功后在当前位置放置炸弹”动作
9.	回合结束
