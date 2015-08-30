package com.huawei.bomber.model;

import com.huawei.bomber.common.Constant;

/**
 * Created by frank on 8/30/15.
 */
public enum DirectionEnum {
    UP(Constant.MOVE_UP(), 0, -1),
    DOWN(Constant.MOVE_DOWN(), 0, 1),
    LEFT(Constant.MOVE_LEFT(), -1, 0),
    RIGHT(Constant.MOVE_RIGHT(), 1, 0);

    public int getDx() {
        return dx;
    }

    public int getDy() {
        return dy;
    }

    public String getName() {
        return name;
    }


    private DirectionEnum(String name, int dx, int dy){
        this.name = name;
        this.dx = dx;
        this.dy = dy;
    }

    private String name;
    private int dx;
    private int dy;

    @Override
    public String toString() {
        return "com.huawei.bomber.model.DirectionEnum{" +
                "name='" + name + '\'' +
                "} " + super.toString();
    }
}