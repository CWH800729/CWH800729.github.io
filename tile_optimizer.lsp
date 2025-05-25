;;; Tile Optimizer - 磁磚最佳化排列程式
;;; 作者：CWH800729
;;; 版本：1.0
;;; 功能：計算並繪製最佳磁磚排列方案

;;; 全域變數定義
(setq *tile-width* 30.0)    ; 磁磚寬度 (cm)
(setq *tile-length* 30.0)   ; 磁磚長度 (cm)
(setq *grout-width* 0.3)    ; 縫寬 (cm)
(setq *wall-height* 240.0)  ; 牆面高度 (cm)

;;; 主要函數：計算最佳排列
(defun calculate-optimal-layout (wall-width wall-height / 
                                tiles-per-row tiles-per-col
                                total-tiles cut-tiles
                                start-offset-x start-offset-y)
  (setq tiles-per-row (fix (/ wall-width *tile-width*)))
  (setq tiles-per-col (fix (/ wall-height *tile-length*)))
  
  ;; 計算起始偏移量（居中對齊）
  (setq start-offset-x (/ (- wall-width (* tiles-per-row *tile-width*)) 2.0))
  (setq start-offset-y (/ (- wall-height (* tiles-per-col *tile-length*)) 2.0))
  
  ;; 計算總用磚量
  (setq total-tiles (* tiles-per-row tiles-per-col))
  
  ;; 計算需要切割的磁磚
  (setq cut-tiles (list
    (if (> (rem wall-width *tile-width*) 0) 1 0)  ; 水平方向切割
    (if (> (rem wall-height *tile-length*) 0) 1 0) ; 垂直方向切割
  ))
  
  ;; 返回結果
  (list
    tiles-per-row
    tiles-per-col
    total-tiles
    cut-tiles
    start-offset-x
    start-offset-y
  )
)

;;; 繪製磁磚排列
(defun draw-tile-layout (start-point wall-width wall-height layout-info / 
                        tiles-per-row tiles-per-col
                        start-offset-x start-offset-y
                        current-x current-y)
  (setq tiles-per-row (nth 0 layout-info))
  (setq tiles-per-col (nth 1 layout-info))
  (setq start-offset-x (nth 4 layout-info))
  (setq start-offset-y (nth 5 layout-info))
  
  ;; 設定當前點為起始點
  (setq current-x (car start-point))
  (setq current-y (cadr start-point))
  
  ;; 繪製磁磚網格
  (repeat tiles-per-col
    (setq current-x (car start-point))
    (repeat tiles-per-row
      ;; 繪製單個磁磚
      (command "._rectangle"
               (list current-x current-y)
               (list (+ current-x *tile-width*) (+ current-y *tile-length*)))
      (setq current-x (+ current-x *tile-width* *grout-width*))
    )
    (setq current-y (+ current-y *tile-length* *grout-width*))
  )
)

;;; 處理門窗開口
(defun handle-openings (openings-list / current-opening)
  (foreach current-opening openings-list
    (let ((type (car current-opening))
          (position (cadr current-opening))
          (width (caddr current-opening))
          (height (cadddr current-opening)))
      
      ;; 根據開口類型繪製
      (cond
        ((= type "DOOR")
         (draw-door position width height))
        ((= type "WINDOW")
         (draw-window position width height))
      )
    )
  )
)

;;; 繪製門
(defun draw-door (position width height / door-points)
  (setq door-points (list
    position
    (list (+ (car position) width) (cadr position))
    (list (+ (car position) width) (+ (cadr position) height))
    (list (car position) (+ (cadr position) height))
  ))
  
  (command "._pline")
  (foreach point door-points
    (command point)
  )
  (command "._close")
)

;;; 繪製窗
(defun draw-window (position width height / window-points)
  (setq window-points (list
    position
    (list (+ (car position) width) (cadr position))
    (list (+ (car position) width) (+ (cadr position) height))
    (list (car position) (+ (cadr position) height))
  ))
  
  (command "._pline")
  (foreach point window-points
    (command point)
  )
  (command "._close")
)

;;; 主命令函數
(defun c:TILEOPT ( / wall-width wall-height
                    start-point openings-list
                    layout-info)
  ;; 獲取使用者輸入
  (setq wall-width (getreal "\n請輸入牆面寬度 (cm): "))
  (setq wall-height (getreal "\n請輸入牆面高度 (cm): "))
  (setq start-point (getpoint "\n請選擇起始點: "))
  
  ;; 計算最佳排列
  (setq layout-info (calculate-optimal-layout wall-width wall-height))
  
  ;; 繪製排列
  (draw-tile-layout start-point wall-width wall-height layout-info)
  
  ;; 顯示結果
  (princ (strcat "\n總用磚量: " (itoa (nth 2 layout-info))))
  (princ (strcat "\n水平切割: " (itoa (car (nth 3 layout-info)))))
  (princ (strcat "\n垂直切割: " (itoa (cadr (nth 3 layout-info)))))
  (princ)
)

;;; 設定命令提示
(prompt "\n輸入 TILEOPT 開始磁磚最佳化排列")
(princ) 