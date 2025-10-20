import SwiftUI
import MetalKit
import UIKit
import CoreMIDI
import ImageIO
import UniformTypeIdentifiers

// MARK: - Mixer Palette (panel + channel colors)
enum MixerPalette {
    static let panel = Color(red: 0.12, green: 0.12, blue: 0.13)
    static let slot  = Color.black.opacity(0.92)
    static let edgeShadow = Color.black.opacity(0.65)
    static let channel: [Color] = [
        Color(red: 0.92, green: 0.29, blue: 0.25), // red
        Color(red: 0.96, green: 0.64, blue: 0.17), // orange
        Color(red: 0.98, green: 0.80, blue: 0.20), // yellow
        Color(red: 0.30, green: 0.69, blue: 0.45), // green
        Color(red: 0.24, green: 0.72, blue: 0.76), // teal
        Color(red: 0.27, green: 0.58, blue: 0.87), // blue
        Color(red: 0.16, green: 0.35, blue: 0.74), // deep blue
        Color(red: 0.54, green: 0.33, blue: 0.73)  // purple
    ]
}

// MARK: - Content
struct ContentView: View {
    // Sliders start at their LOWER BOUNDS (practical “zero position”)
    @State private var frequency:   Float = 1.0    // range: 1...25
    @State private var speed:       Float = 0.1    // range: 0.1...5
    @State private var glow:        Float = 0.001  // range: 0.001...0.05
    @State private var colorShift:  Float = 0.0    // range: 0...3
    @State private var zoom:        Float = 0.5    // range: 0.5...3
    @State private var iterations:  Float = 1.0    // range: 1...6
    @State private var hueShift:    Float = 0.0    // range: 0...2
    @State private var symmetry:    Float = 0.5    // range: 0.5...3
    
    // Snapshots & sharing
    @State private var snapshot: UIImage?
    @State private var showShareSheet = false
    @State private var shareItems: [Any] = []
    
    @StateObject private var midi = MIDIManager()
    
    // Knobs (0…1) start at 0
    @State private var knobPositions: [Double] = Array(repeating: 0.0, count: 8)
    @State private var padPressed: [Bool] = Array(repeating: false, count: 8)
    
    // Split view (used on iPad + iPhone portrait only)
    @State private var mixerFraction: CGFloat = 0.33
    @State private var lastMixerFraction: CGFloat = 0.33
    @State private var dragStartFraction: CGFloat = 0.33
    @State private var isDraggingSplit: Bool = false
    @State private var mixerVisible: Bool = true
    
    // Full-screen presentation
    @State private var isFullScreen: Bool = false
    
    // ─── Knob mappings ──────────────────────────────────────────────────────
    private var swirlVal:      Float { Float(knobPositions[0]) * 2.5 }
    private var rippleVal:     Float { Float(knobPositions[1]) * 0.35 }
    private var warpVal:       Float { Float(knobPositions[2]) * 0.50 }
    private var twistVal:      Float { Float(knobPositions[3]) * 2.0 }
    private var curlStrengthVal: Float { Float(knobPositions[4]) * 1.20 }
    private var echoVal:         Float { Float(knobPositions[5]) * 0.90 }
    private var hueKnobOffset:   Float { Float(knobPositions[6]) * 2.0 - 1.0 } // -1..+1
    private var effectiveHueShift: Float { max(0, min(2, hueShift + hueKnobOffset)) }
    private var cellNoiseVal:    Float { Float(knobPositions[7]) * 1.00 }
    private var flowAngleVal:    Float { 0.0 }
    
    // Size classes
    @Environment(\.horizontalSizeClass) private var hSize
    @Environment(\.verticalSizeClass) private var vSize
    
    var body: some View {
        GeometryReader { geo in
            // Device/orientation
            let isPhone = UIDevice.current.userInterfaceIdiom == .phone
            let isLandscape = geo.size.width > geo.size.height
            let isPhoneLandscape = isPhone && isLandscape
            
            let totalH = geo.size.height
            let totalW = geo.size.width
            let handleH: CGFloat = 22
            let safeBottom = geo.safeAreaInsets.bottom
            
            // Portrait/iPad metrics
            let isCompactWidth  = (hSize == .compact) || geo.size.width < 700
            let isCompactHeight = (vSize == .compact) || geo.size.height < 500
            let minMixerH: CGFloat = (isCompactHeight ? 120 : 240) + safeBottom
            let maxMixerH: CGFloat = Swift.max(isCompactHeight ? totalH * 0.92 : totalH * 0.78,
                                               isCompactHeight ? 200 : 340)
            let wantedMixerH = totalH * mixerFraction
            let mixerH = mixerVisible ? Swift.min(Swift.max(wantedMixerH, minMixerH), maxMixerH) : 0
            let shaderH = Swift.max(0, totalH - mixerH - (mixerVisible ? handleH : 0))
            
            ZStack {
                // iPHONE LANDSCAPE: side‑by‑side
                if isPhoneLandscape {
                    let mixerW = min(max(totalW * 0.42, 310), totalW * 0.55)
                    HStack(spacing: 0) {
                        MixerArea(
                            height: totalH,
                            isCompactWidth: true,
                            isCompactHeight: isCompactHeight,
                            frequency: $frequency, speed: $speed, glow: $glow, colorShift: $colorShift,
                            zoom: $zoom, iterations: $iterations, hueShift: $hueShift, symmetry: $symmetry,
                            knobPositions: $knobPositions, padPressed: $padPressed,
                            exportPNG: { exportImage(as: "png") },
                            exportJPEG: { exportImage(as: "jpeg") },
                            exportShader: { exportShader() },
                            exportGIF: { exportAnimatedGIF() },
                            safeBottom: safeBottom
                        )
                        .frame(width: mixerW)
                        .background(MixerPalette.panel)
                        
                        Rectangle().fill(Color.black.opacity(0.25)).frame(width: 1)
                        
                        MetalShaderView(
                            frequency: frequency,
                            speed: speed,
                            glow: glow,
                            colorShift: colorShift,
                            zoom: zoom,
                            iterations: iterations,
                            hueShift: effectiveHueShift,
                            symmetry: symmetry,
                            swirl: swirlVal, ripple: rippleVal, warp: warpVal, twist: twistVal,
                            curlStrength: curlStrengthVal, echo: echoVal, flowAngle: flowAngleVal, cellNoise: cellNoiseVal,
                            snapshot: $snapshot
                        )
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                        .contentShape(Rectangle())
                        .onTapGesture {
                            withAnimation(.spring(response: 0.35, dampingFraction: 0.85)) {
                                isFullScreen = true
                            }
                        }
                    }
                } else {
                    // PORTRAIT (iPhone) + iPAD: vertical with draggable handle
                    VStack(spacing: 0) {
                        MetalShaderView(
                            frequency: frequency,
                            speed: speed,
                            glow: glow,
                            colorShift: colorShift,
                            zoom: zoom,
                            iterations: iterations,
                            hueShift: effectiveHueShift,
                            symmetry: symmetry,
                            swirl: swirlVal, ripple: rippleVal, warp: warpVal, twist: twistVal,
                            curlStrength: curlStrengthVal, echo: echoVal, flowAngle: flowAngleVal, cellNoise: cellNoiseVal,
                            snapshot: $snapshot
                        )
                        .frame(height: shaderH)
                        .frame(maxWidth: .infinity)
                        .contentShape(Rectangle())
                        .onTapGesture {
                            withAnimation(.spring(response: 0.35, dampingFraction: 0.85)) {
                                lastMixerFraction = mixerFraction
                                isFullScreen = true
                            }
                        }
                        
                        if mixerVisible {
                            SplitHandle(isDragging: isDraggingSplit)
                                .frame(height: handleH)
                                .frame(maxWidth: .infinity)
                                .background(
                                    ZStack {
                                        Color.black.opacity(0.15)
                                        LinearGradient(colors: [.black.opacity(0.2), .clear],
                                                       startPoint: .top, endPoint: .bottom)
                                    }
                                )
                                .gesture(
                                    DragGesture(minimumDistance: 0)
                                        .onChanged { g in
                                            if !isDraggingSplit {
                                                isDraggingSplit = true
                                                dragStartFraction = mixerFraction
                                            }
                                            let delta = g.translation.height / totalH
                                            let minF = minMixerH / totalH
                                            let maxF = maxMixerH / totalH
                                            let newF = dragStartFraction - delta
                                            mixerFraction = Swift.min(Swift.max(newF, minF), maxF)
                                        }
                                        .onEnded { _ in isDraggingSplit = false }
                                )
                        }
                        
                        if mixerVisible {
                            MixerArea(
                                height: mixerH,
                                isCompactWidth: isCompactWidth,
                                isCompactHeight: isCompactHeight,
                                frequency: $frequency, speed: $speed, glow: $glow, colorShift: $colorShift,
                                zoom: $zoom, iterations: $iterations, hueShift: $hueShift, symmetry: $symmetry,
                                knobPositions: $knobPositions, padPressed: $padPressed,
                                exportPNG: { exportImage(as: "png") },
                                exportJPEG: { exportImage(as: "jpeg") },
                                exportShader: { exportShader() },
                                exportGIF: { exportAnimatedGIF() },
                                safeBottom: safeBottom
                            )
                            .transition(.move(edge: .bottom).combined(with: .opacity))
                        }
                    }
                }
            }
            .background(MixerPalette.panel)
            .fullScreenCover(isPresented: $isFullScreen, onDismiss: {
                mixerVisible = true
                mixerFraction = lastMixerFraction
            }) {
                FullScreenHost {
                    ShaderFullscreen(
                        frequency: frequency, speed: speed, glow: glow, colorShift: colorShift,
                        zoom: zoom, iterations: iterations, hueShift: effectiveHueShift, symmetry: symmetry,
                        swirl: swirlVal, ripple: rippleVal, warp: warpVal, twist: twistVal,
                        curlStrength: curlStrengthVal, echo: echoVal, flowAngle: flowAngleVal, cellNoise: cellNoiseVal,
                        snapshot: $snapshot
                    )
                }
            }
        }
        .sheet(isPresented: $showShareSheet) { ShareSheet(activityItems: shareItems) }
        .onAppear { setupMIDI() }
        .onDisappear { midi.stop() }
    }
    
    // MARK: - MIDI + helpers
    private let speedSteps: Int = 12 // quantized speed for smoother feel
    
    private func setupMIDI() {
        midi.onControlChange = { cc, val in
            let u = Float(val) / 127.0
            let icc = Int(cc)
            
            DispatchQueue.main.async {
                if (16...23).contains(icc) { knobPositions[icc - 16] = Double(u) }
                
                if let pad = padIndex(forCC: icc), val > 0 {
                    pulsePad(pad)
                    triggerPad(pad)
                }
                
                // Faders (CC 0…7) with quantized SPEED
                switch icc {
                case 0: frequency = lerp(1, 25, u)
                case 1:
                    let raw = lerp(0.1, 5, u)
                    speed = quantize(raw, in: 0.1...5.0, steps: speedSteps)
                case 2: glow = lerp(0.001, 0.05, u)
                case 3: colorShift = lerp(0, 3, u)
                case 4: zoom = lerp(0.5, 3, u)
                case 5: iterations = lerp(1, 6, u)
                case 6: hueShift = lerp(0, 2, u)
                case 7: symmetry = lerp(0.5, 3, u)
                default: break
                }
            }
        }
        midi.start()
    }
    
    private func padIndex(forCC cc: Int) -> Int? {
        if (32...39).contains(cc) { return cc - 32 } // Solo
        if (48...55).contains(cc) { return cc - 48 } // Mute
        if (64...71).contains(cc) { return cc - 64 } // Record
        return nil
    }
    
    private func pulsePad(_ i: Int) {
        guard padPressed.indices.contains(i) else { return }
        padPressed[i] = true
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.12) { padPressed[i] = false }
    }
    
    private func triggerPad(_ i: Int) {
        if i == 2 { exportShader() }    // yellow pad exports shader files
        else if i == 3 { exportImage(as: "png") }
        else if i == 4 { exportImage(as: "jpeg") }
        else if i == 5 { exportAnimatedGIF() } // GIF export on pad #6
    }
    
    private func lerp(_ a: Float, _ b: Float, _ t: Float) -> Float { a + (b - a) * t }
    private func quantize(_ v: Float, in range: ClosedRange<Float>, steps: Int) -> Float {
        guard steps > 0 else { return v }
        let t = (v - range.lowerBound) / (range.upperBound - range.lowerBound)
        let q = round(t * Float(steps)) / Float(steps)
        return range.lowerBound + q * (range.upperBound - range.lowerBound)
    }
    
    // MARK: - Export (images)
    private func exportImage(as type: String) {
        guard let image = snapshot else { return }
        let data = (type == "png") ? image.pngData() : image.jpegData(compressionQuality: 0.95)
        guard let data = data else { return }
        let url = FileManager.default.temporaryDirectory.appendingPathComponent("shader_art.\(type)")
        try? data.write(to: url)
        shareItems = [url]
        showShareSheet = true
    }
    
    // MARK: - Export (shader text files)
    private func exportShader() {
        let godotSource = generateGodotShader()
        let unitySource = generateUnityShader()
        let dir = FileManager.default.temporaryDirectory
        let godotURL = dir.appendingPathComponent("Shader_Export.gdshader")
        let unityURL = dir.appendingPathComponent("Shader_Export_Unity.shader")
        do {
            try godotSource.write(to: godotURL, atomically: true, encoding: .utf8)
            try unitySource.write(to: unityURL, atomically: true, encoding: .utf8)
            shareItems = [godotURL, unityURL]
        } catch {
            let txtURL = dir.appendingPathComponent("Shader_Export.txt")
            try? (godotSource + "\n\n===== UNITY =====\n\n" + unitySource)
                .write(to: txtURL, atomically: true, encoding: .utf8)
            shareItems = [txtURL]
        }
        showShareSheet = true
    }
    
    private func fmt(_ v: Float) -> String { String(format: "%.6f", v) }
    
    // ---------- Godot shader text ----------
    private func generateGodotShader() -> String {
        """
        // Exported from iPad Shader Mixer — Godot 4 CanvasItem Shader
        shader_type canvas_item;
        
        uniform float frequency   = \(fmt(frequency));
        uniform float speed       = \(fmt(speed));
        uniform float glow        = \(fmt(glow));
        uniform float colorShift  = \(fmt(colorShift));
        uniform float zoom        = \(fmt(zoom));
        uniform float iterations  = \(fmt(iterations));
        uniform float hueShift    = \(fmt(effectiveHueShift));
        uniform float symmetry    = \(fmt(symmetry));
        
        uniform float swirl       = \(fmt(swirlVal));
        uniform float ripple      = \(fmt(rippleVal));
        uniform float warpAmt     = \(fmt(warpVal));
        uniform float twist       = \(fmt(twistVal));
        
        uniform float curlStrength = \(fmt(curlStrengthVal));
        uniform float echoAmt      = \(fmt(echoVal));
        uniform float flowAngle    = 0.0;
        uniform float cellNoiseAmt = \(fmt(cellNoiseVal));
        
        float2 rotate2D(float2 p, float a){float s=sin(a),c=cos(a);return float2(c*p.x-s*p.y,s*p.x+c*p.y);}
        float2 hash22(float2 p){p=float2(dot(p,float2(127.1,311.7)),dot(p,float2(269.5,183.3)));return fract(sin(p)*43758.5453);}
        float noise2d(float2 x){float2 i=floor(x);float2 f=fract(x);float2 u=f*f*(3.0-2.0*f);
            float2 a=hash22(i+float2(0.0,0.0));float2 b=hash22(i+float2(1.0,0.0));
            float2 c=hash22(i+float2(0.0,1.0));float2 d=hash22(i+float2(1.0,1.0));
            float n00=dot(a,f-float2(0.0,0.0));float n10=dot(b,f-float2(1.0,0.0));
            float n01=dot(c,f-float2(0.0,1.0));float n11=dot(d,f-float2(1.0,1.0));
            return mix(mix(n00,n10,u.x),mix(n01,n11,u.x),u.y);}
        float fbm(float2 p){float v=0.0,a=0.5;for(int i=0;i<4;i++){v+=a*noise2d(p);p*=2.0;a*=0.5;}return v;}
        float worley(float2 p){float2 ip=floor(p),fp=fract(p);float minD=10.0;
            for(int j=-1;j<=1;++j){for(int i=-1;i<=1;++i){float2 g=float2(float(i),float(j));
                float2 o=hash22(ip+g);float2 d=g+o-fp;minD=min(minD,length(d));}}return minD;}
        float2 gradWorley(float2 p){float e=0.005;float dx=worley(p+float2(e,0.0))-worley(p-float2(e,0.0));
            float dy=worley(p+float2(0.0,e))-worley(p-float2(0.0,e));return float2(dx,dy)/(2.0*e);}
        float2 gradFBM(float2 p){float e=0.005;float dx=fbm(p+float2(e,0.0))-fbm(p-float2(e,0.0));
            float dy=fbm(p+float2(0.0,e))-fbm(p-float2(0.0,e));return float2(dx,dy)/(2.0*e);}
        float2 curlField(float2 p){float2 g=gradFBM(p);return float2(g.y,-g.x);}
        
        float3 sceneCore(float2 uv,float time,float freq,float glowVal,float colShift,float iter,float hue,float sym){
            float2 uv0=uv;float3 c=float3(0.0);int N=int(round(iter));
            for(int ii=0;ii<N;ii++){float i=float(ii);float2 u=fract(uv*sym)-0.5;float d=length(u)*exp(-length(uv0));
                float3 col=0.5+0.5*cos(float3(0.7,0.6,0.5)*(length(uv0)+colShift+i+time*0.4+hue));
                d=sin(d*freq+time*1.2)/8.0;d=abs(d);d=glowVal/d;c+=col*d;uv=u;}return c;}
        
        float2 preTransform(float2 uv,float t,float freq,float sw,float rp,float wp,float tw){
            float r=length(uv);float2 dir=(r>1e-5)?uv/r:float2(0.0,1.0);float theta=atan(uv.y,uv.x);
            float ang=sw*r*r+tw*sin(3.0*theta)*r;uv=rotate2D(uv,ang);
            uv+=dir*(sin(r*(6.0+freq*1.5)-t*3.0)*rp);
            uv+=wp*float2(sin(uv.y*3.0+t*1.3),cos(uv.x*3.0-t*1.1));return uv;}
        
        void fragment(){
            vec2 res=1.0/SCREEN_PIXEL_SIZE;
            vec2 uv=(UV-0.5)*2.0; uv.x*=res.x/res.y; uv*=zoom;
            float t=TIME*speed;
        
            if(curlStrength>0.001){vec2 cv=curlField(uv*1.4+vec2(cos(t*1.2)*0.6,sin(t*0.9)*0.6)); uv+=curlStrength*0.15*cv;}
            if(cellNoiseAmt>0.001){vec2 gw=gradWorley(uv*2.0+vec2(cos(t*1.8)*0.6,sin(t*1.1)*0.6));
                vec2 nrm=normalize(vec2(gw.y,-gw.x)+1e-6); uv+=cellNoiseAmt*0.12*nrm;}
        
            vec2 baseUV=preTransform(uv,t,frequency,swirl,ripple,warpAmt,twist);
            vec3 c=sceneCore(baseUV,t,frequency,glow,colorShift,iterations,hueShift,symmetry);
        
            if(echoAmt>0.001){
                vec2 flowDir=vec2(cos(flowAngle),sin(flowAngle));
                float stepLen=0.05+0.18*echoAmt;
                float w1=0.45*echoAmt,w2=0.25*echoAmt,w3=0.15*echoAmt,normW=1.0+(w1+w2+w3);
                float t1=t-0.08,t2=t-0.16,t3=t-0.24;
                vec2 uv1=preTransform(uv-flowDir*stepLen,t1,frequency,swirl,ripple,warpAmt,twist);
                vec2 uv2=preTransform(uv-flowDir*stepLen*2.0,t2,frequency,swirl,ripple,warpAmt,twist);
                vec2 uv3=preTransform(uv-flowDir*stepLen*3.0,t3,frequency,swirl,ripple,warpAmt,twist);
                vec3 c1=sceneCore(uv1,t1,frequency,glow,colorShift,iterations,hueShift,symmetry);
                vec3 c2=sceneCore(uv2,t2,frequency,glow,colorShift,iterations,hueShift,symmetry);
                vec3 c3=sceneCore(uv3,t3,frequency,glow,colorShift,iterations,hueShift,symmetry);
                c=(c+c1*w1+c2*w2+c3*w3)/normW;
            }
        
            c=pow(c,vec3(1.2)); COLOR=vec4(c,1.0);
        }
        """
    }
    
    // ---------- Unity shader text ----------
    private func generateUnityShader() -> String {
        """
        // Exported from iPad Shader Mixer — Unity Unlit Shader
        Shader "Unlit/ShaderExport"
        {
            Properties
            {
                _Frequency("Frequency", Float) = \(fmt(frequency))
                _Speed("Speed", Float)         = \(fmt(speed))
                _Glow("Glow", Float)           = \(fmt(glow))
                _ColorShift("Color Shift", Float) = \(fmt(colorShift))
                _Zoom("Zoom", Float)           = \(fmt(zoom))
                _Iterations("Iterations", Float) = \(fmt(iterations))
                _HueShift("Hue Shift", Float)  = \(fmt(effectiveHueShift))
                _Symmetry("Symmetry", Float)   = \(fmt(symmetry))
                _Swirl("Swirl", Float)         = \(fmt(swirlVal))
                _Ripple("Ripple", Float)       = \(fmt(rippleVal))
                _WarpAmt("Warp", Float)        = \(fmt(warpVal))
                _Twist("Twist", Float)         = \(fmt(twistVal))
                _CurlStrength("Curl Strength", Float) = \(fmt(curlStrengthVal))
                _EchoAmt("Echo", Float)        = \(fmt(echoVal))
                _FlowAngle("Flow Angle", Float)= 0.0
                _CellNoiseAmt("Cell Noise", Float) = \(fmt(cellNoiseVal))
            }
            SubShader
            {
                Tags{ "RenderType"="Opaque" "Queue"="Geometry" }
                Cull Off ZWrite Off ZTest Always
        
                Pass
                {
                    CGPROGRAM
                    #pragma vertex vert
                    #pragma fragment frag
                    #include "UnityCG.cginc"
        
                    float _Frequency,_Speed,_Glow,_ColorShift,_Zoom,_Iterations,_HueShift,_Symmetry;
                    float _Swirl,_Ripple,_WarpAmt,_Twist,_CurlStrength,_EchoAmt,_FlowAngle,_CellNoiseAmt;
        
                    struct appdata { float4 vertex:POSITION; float2 uv:TEXCOORD0; };
                    struct v2f { float4 pos:SV_POSITION; float2 uv:TEXCOORD0; };
        
                    v2f vert(appdata v){ v2f o; o.pos = UnityObjectToClipPos(v.vertex); o.uv=v.uv; return o; }
        
                    float2 rotate2D(float2 p,float a){float s=sin(a),c=cos(a);return float2(c*p.x-s*p.y,s*p.x+c*p.y);}
                    float2 hash22(float2 p){p=float2(dot(p,float2(127.1,311.7)),dot(p,float2(269.5,183.3)));return frac(sin(p)*43758.5453);}
                    float noise2d(float2 x){float2 i=floor(x);float2 f=frac(x);float2 u=f*f*(3.0-2.0*f);
                        float2 a=hash22(i+float2(0,0)),b=hash22(i+float2(1,0)),c=hash22(i+float2(0,1)),d=hash22(i+float2(1,1));
                        float n00=dot(a,f-float2(0,0)),n10=dot(b,f-float2(1,0)),n01=dot(c,f-float2(0,1)),n11=dot(d,f-float2(1,1));
                        return lerp(lerp(n00,n10,u.x),lerp(n01,n11,u.x),u.y);}
                    float fbm(float2 p){float v=0,a=0.5;[unroll]for(int i=0;i<4;i++){v+=a*noise2d(p);p*=2;a*=0.5;}return v;}
                    float worley(float2 p){float2 ip=floor(p),fp=frac(p);float minD=10;
                        for(int j=-1;j<=1;j++){for(int i=-1;i<=1;i++){float2 g=float2((float)i,(float)j);
                            float2 o=hash22(ip+g);float2 d=g+o-fp;minD=min(minD,length(d));}}return minD;}
                    float2 gradWorley(float2 p){float e=0.005;float dx=worley(p+float2(e,0))-worley(p-float2(e,0));
                        float dy=worley(p+float2(0,e))-worley(p-float2(0,e));return float2(dx,dy)/(2.0*e);}
                    float2 gradFBM(float2 p){float e=0.005;float dx=fbm(p+float2(e,0))-fbm(p-float2(e,0));
                        float dy=fbm(p+float2(0,e))-fbm(p-float2(0,e));return float2(dx,dy)/(2.0*e);}
                    float2 curlField(float2 p){float2 g=gradFBM(p);return float2(g.y,-g.x);}
        
                    float3 sceneCore(float2 uv,float time,float freq,float glowVal,float colShift,float iter,float hue,float sym){
                        float2 uv0=uv;float3 c=float3(0,0,0);int N=(int)round(iter);
                        [loop]for(int ii=0;ii<N;++ii){float i=(float)ii;float2 u=frac(uv*sym)-0.5;float d=length(u)*exp(-length(uv0));
                            float3 col=0.5+0.5*cos(float3(0.7,0.6,0.5)*(length(uv0)+colShift+i+time*0.4+hue));
                            d=sin(d*freq+time*1.2)/8.0; d=abs(d); d=glowVal/d; c+=col*d; uv=u;} return c; }
        
                    float2 preTransform(float2 uv,float t,float freq,float sw,float rp,float wp,float tw){
                        float r=length(uv); float2 dir=(r>1e-5)?uv/r:float2(0,1); float theta=atan2(uv.y,uv.x);
                        float ang=sw*r*r+tw*sin(3.0*theta)*r; uv=rotate2D(uv,ang);
                        uv+=dir*(sin(r*(6.0+freq*1.5)-t*3.0)*rp);
                        uv+=wp*float2(sin(uv.y*3.0+t*1.3),cos(uv.x*3.0-t*1.1)); return uv;
                    }
        
                    fixed4 frag(v2f i):SV_Target{
                        float2 uv=(i.uv-0.5)*2.0; uv.x*=_ScreenParams.x/_ScreenParams.y; uv*=_Zoom;
                        float t=_Time.y*_Speed;
        
                        if(_CurlStrength>0.001){float2 cv=curlField(uv*1.4+float2(cos(t*1.2)*0.6,sin(t*0.9)*0.6)); uv+=_CurlStrength*0.15*cv;}
                        if(_CellNoiseAmt>0.001){float2 gw=gradWorley(uv*2.0+float2(cos(t*1.8)*0.6,sin(t*1.1)*0.6));
                            float2 nrm=normalize(float2(gw.y,-gw.x)+1e-6); uv+=_CellNoiseAmt*0.12*nrm;}
        
                        float2 baseUV=preTransform(uv,t,_Frequency,_Swirl,_Ripple,_WarpAmt,_Twist);
                        float3 c=sceneCore(baseUV,t,_Frequency,_Glow,_ColorShift,_Iterations,_HueShift,_Symmetry);
        
                        if(_EchoAmt>0.001){
                            float2 flowDir=float2(cos(_FlowAngle),sin(_FlowAngle));
                            float stepLen=0.05+0.18*_EchoAmt;
                            float w1=0.45*_EchoAmt,w2=0.25*_EchoAmt,w3=0.15*_EchoAmt,normW=1.0+(w1+w2+w3);
                            float t1=t-0.08,t2=t-0.16,t3=t-0.24;
                            float2 uv1=preTransform(uv-flowDir*stepLen,t1,_Frequency,_Swirl,_Ripple,_WarpAmt,_Twist);
                            float2 uv2=preTransform(uv-flowDir*stepLen*2.0,t2,_Frequency,_Swirl,_Ripple,_WarpAmt,_Twist);
                            float2 uv3=preTransform(uv-flowDir*stepLen*3.0,t3,_Frequency,_Swirl,_Ripple,_WarpAmt,_Twist);
                            float3 c1=sceneCore(uv1,t1,_Frequency,_Glow,_ColorShift,_Iterations,_HueShift,_Symmetry);
                            float3 c2=sceneCore(uv2,t2,_Frequency,_Glow,_ColorShift,_Iterations,_HueShift,_Symmetry);
                            float3 c3=sceneCore(uv3,t3,_Frequency,_Glow,_ColorShift,_Iterations,_HueShift,_Symmetry);
                            c=(c+c1*w1+c2*w2+c3*w3)/normW;
                        }
        
                        c=pow(c,float3(1.2,1.2,1.2)); return float4(c,1.0);
                    }
                    ENDCG
                }
            }
        }
        """
    }
    
    // MARK: - Smooth, perfectly-looped GIF export
    // Runs off-main, caps frames, uses GIF-safe timing.
    private func exportAnimatedGIF(
        side: Int = 768,       // square output size
        fps: Int = 50,         // 10/20/25/50 are safest for GIF
        maxFrames: Int = 240,  // cap for performance (50 fps → ~4.8 s)
        temporalSamples: Int = 1 // increase to 2 for slight motion blur
    ) {
        Task.detached(priority: .userInitiated) {
            let fpsSafe = [10, 20, 25, 50].min(by: { abs($0 - fps) < abs($1 - fps) }) ?? 50
            let frameDelayCS = 100 / fpsSafe
            let frameDelay = Double(frameDelayCS) / 100.0
            
            // one full internal period T -> perfect loop irrespective of fps/frames
            let T: Float = 2 * .pi * 10
            let frames = max(1, min(maxFrames, fpsSafe * 10 / 2)) // simple heuristic cap
            let dtT: Float = T / Float(frames)
            
            // Parameters snapshot
            let cfg = ExportConfig(
                frequency: frequency, speed: speed, glow: glow, colorShift: colorShift,
                zoom: zoom, iterations: iterations, hueShift: effectiveHueShift, symmetry: symmetry,
                swirl: swirlVal, ripple: rippleVal, warp: warpVal, twist: twistVal,
                curlStrength: curlStrengthVal, echo: echoVal, flowAngle: flowAngleVal, cellNoise: cellNoiseVal
            )
            
            guard let renderer = OffscreenRenderer() else { return }
            
            // Prepare destination
            let url = FileManager.default.temporaryDirectory.appendingPathComponent("shader_loop.gif")
            guard let dest = CGImageDestinationCreateWithURL(url as CFURL,
                                                             UTType.gif.identifier as CFString,
                                                             frames, nil) else { return }
            CGImageDestinationSetProperties(dest, [kCGImagePropertyGIFDictionary: [kCGImagePropertyGIFLoopCount: 0]] as CFDictionary)
            
            let samples = max(1, min(4, temporalSamples))
            let rCurl: Float = 0.6, rCell: Float = 0.6
            
            for i in 0..<frames {
                autoreleasepool {
                    let u = Float(i) / Float(frames)
                    let t0: Float = Float(i) * dtT
                    // circular noise offsets: close exactly at frame N
                    let phi = 2 * Float.pi * u
                    let noiseCurl = SIMD2<Float>(rCurl * cos(phi * 12), rCurl * sin(phi * 9))
                    let noiseCell = SIMD2<Float>(rCell * cos(phi * 18), rCell * sin(phi * 11))
                    
                    let cg: CGImage?
                    if samples == 1 {
                        cg = renderer.render(size: side, time: t0, params: cfg, noiseCurl: noiseCurl, noiseCell: noiseCell)
                    } else {
                        // simple temporal supersample blend
                        var accum = [Float](repeating: 0, count: side * side * 4)
                        for s in 0..<samples {
                            let tSub = t0 + (Float(s) + 0.5) / Float(samples) * dtT
                            if let bytes = renderer.renderBytes(size: side, time: tSub, params: cfg, noiseCurl: noiseCurl, noiseCell: noiseCell) {
                                var idx = 0
                                let w: Float = 1.0 / Float(samples)
                                for _ in 0..<(side * side) {
                                    accum[idx+0] += Float(bytes[idx+0]) * w
                                    accum[idx+1] += Float(bytes[idx+1]) * w
                                    accum[idx+2] += Float(bytes[idx+2]) * w
                                    accum[idx+3]  = 255
                                    idx += 4
                                }
                            }
                        }
                        var out = [UInt8](repeating: 255, count: side * side * 4)
                        var j = 0
                        for _ in 0..<(side * side) {
                            out[j+0] = UInt8(min(255, max(0, accum[j+0])) + 0.5)
                            out[j+1] = UInt8(min(255, max(0, accum[j+1])) + 0.5)
                            out[j+2] = UInt8(min(255, max(0, accum[j+2])) + 0.5)
                            out[j+3] = 255
                            j += 4
                        }
                        cg = OffscreenRenderer.imageFromBytes(out, width: side, height: side)
                    }
                    
                    if let cg = cg {
                        let frameProps: CFDictionary = [
                            kCGImagePropertyGIFDictionary: [
                                kCGImagePropertyGIFUnclampedDelayTime: frameDelay,
                                kCGImagePropertyGIFDelayTime: frameDelay
                            ]
                        ] as CFDictionary
                        CGImageDestinationAddImage(dest, cg, frameProps)
                    }
                }
            }
            
            guard CGImageDestinationFinalize(dest) else { return }
            
            await MainActor.run {
                shareItems = [url]
                showShareSheet = true
            }
        }
    }
}

// MARK: - Full-screen shader screen
struct ShaderFullscreen: View {
    var frequency, speed, glow, colorShift, zoom, iterations, hueShift, symmetry: Float
    var swirl, ripple, warp, twist, curlStrength, echo, flowAngle, cellNoise: Float
    @Binding var snapshot: UIImage?
    
    @Environment(\.dismiss) private var dismiss
    
    var body: some View {
        MetalShaderView(
            frequency: frequency, speed: speed, glow: glow, colorShift: colorShift,
            zoom: zoom, iterations: iterations, hueShift: hueShift, symmetry: symmetry,
            swirl: swirl, ripple: ripple, warp: warp, twist: twist,
            curlStrength: curlStrength, echo: echo, flowAngle: flowAngle, cellNoise: cellNoise,
            snapshot: $snapshot
        )
        .ignoresSafeArea()
        .contentShape(Rectangle())
        .onTapGesture { dismiss() }
        .background(Color.black)
    }
}

// MARK: - A hosting controller that hides system bars for full-screen
struct FullScreenHost<Content: View>: UIViewControllerRepresentable {
    var content: Content
    init(@ViewBuilder content: () -> Content) { self.content = content() }
    
    func makeUIViewController(context: Context) -> Hosting { Hosting(rootView: content) }
    func updateUIViewController(_ vc: Hosting, context: Context) { vc.rootView = content }
    
    final class Hosting: UIHostingController<Content> {
        override var prefersStatusBarHidden: Bool { true }
        override var prefersHomeIndicatorAutoHidden: Bool { true }
        override var preferredScreenEdgesDeferringSystemGestures: UIRectEdge { .all }
    }
}

// MARK: - Split Handle (grabber)
struct SplitHandle: View {
    var isDragging: Bool
    var body: some View {
        ZStack {
            Capsule()
                .fill(Color.white.opacity(isDragging ? 0.8 : 0.45))
                .frame(width: 50, height: 6)
                .shadow(color: .black.opacity(0.4), radius: 2, x: 0, y: 1)
        }
        .frame(maxWidth: .infinity)
        .background(Color.black.opacity(0.15))
    }
}

// MARK: - Layout Metrics Helper
private struct MixerMetrics {
    let sliderWidth: CGFloat
    let knobSize: CGFloat
    let padSize: CGFloat
    let sliderHeight: CGFloat
    let topPadding: CGFloat
    let bottomPadding: CGFloat
    let gapKnobToSlider: CGFloat
    let gapSliderToPad: CGFloat
    let gridSpacing: CGFloat
    let horizontalPadding: CGFloat
    let columnsFixed: [GridItem]
    
    static func compute(h: CGFloat,
                        w: CGFloat,
                        safeBottom: CGFloat,
                        isCompactWidth: Bool,
                        isCompactHeight: Bool) -> MixerMetrics {
        let spacing: CGFloat = isCompactWidth ? 8 : 14
        let padding: CGFloat = isCompactWidth ? 10 : 16
        
        let available = max(0, w - 2*padding - spacing * 7)
        let sliderWidth = floor(max(34, available / 8))
        
        var knob = min(max(sliderWidth * (isCompactWidth ? 0.80 : 0.90), isCompactWidth ? 28 : 36),
                       isCompactWidth ? 44 : 68)
        var pad  = min(max(sliderWidth * (isCompactWidth ? 0.85 : 0.95), isCompactWidth ? 32 : 44),
                       isCompactWidth ? 50 : 74)
        
        let topPadding: CGFloat = max(6, h * 0.015)
        let bottomPadding: CGFloat = max(10, h * 0.02) + safeBottom + 8
        let gapKnobToSlider: CGFloat = max(4, h * 0.012)
        let gapSliderToPad: CGFloat  = max(4, h * 0.012)
        
        var sliderHeight = h - (topPadding + knob + gapKnobToSlider + pad + gapSliderToPad + bottomPadding)
        
        let minSlider: CGFloat = isCompactHeight ? 72 : 96
        if sliderHeight < minSlider {
            let deficit = minSlider - sliderHeight
            let reduceEach = deficit / 2
            knob = max(isCompactWidth ? 24 : 30, knob - reduceEach)
            pad  = max(isCompactWidth ? 28 : 36, pad  - reduceEach)
            sliderHeight = h - (topPadding + knob + gapKnobToSlider + pad + gapSliderToPad + bottomPadding)
        }
        sliderHeight = max(minSlider, sliderHeight)
        
        let fixed = (0..<8).map { _ in GridItem(.fixed(sliderWidth), spacing: spacing) }
        
        return MixerMetrics(
            sliderWidth: sliderWidth,
            knobSize: knob,
            padSize: pad,
            sliderHeight: sliderHeight,
            topPadding: topPadding,
            bottomPadding: bottomPadding,
            gapKnobToSlider: gapKnobToSlider,
            gapSliderToPad: gapSliderToPad,
            gridSpacing: spacing,
            horizontalPadding: padding,
            columnsFixed: fixed
        )
    }
}

// MARK: - Mixer Area
struct MixerArea: View {
    var height: CGFloat
    var isCompactWidth: Bool
    var isCompactHeight: Bool
    
    @Binding var frequency: Float
    @Binding var speed: Float
    @Binding var glow: Float
    @Binding var colorShift: Float
    @Binding var zoom: Float
    @Binding var iterations: Float
    @Binding var hueShift: Float
    @Binding var symmetry: Float
    
    @Binding var knobPositions: [Double]
    @Binding var padPressed: [Bool]
    
    var exportPNG: () -> Void
    var exportJPEG: () -> Void
    var exportShader: () -> Void
    var exportGIF: () -> Void
    var safeBottom: CGFloat
    
    var body: some View {
        GeometryReader { geo in
            let W = geo.size.width
            let m = MixerMetrics.compute(
                h: height, w: W, safeBottom: safeBottom,
                isCompactWidth: isCompactWidth, isCompactHeight: isCompactHeight
            )
            
            VStack(spacing: 0) {
                // --- Top: Knobs (8 fixed columns) ---
                LazyVGrid(columns: m.columnsFixed, alignment: .center, spacing: 12) {
                    ForEach(0..<8, id: \.self) { i in
                        RotaryKnob(
                            progress: Binding(
                                get: { knobPositions[i] },
                                set: { knobPositions[i] = $0 }
                            ),
                            diameter: m.knobSize,
                            color: MixerPalette.channel[i]
                        )
                        .frame(width: m.sliderWidth)
                    }
                }
                .padding(.horizontal, m.horizontalPadding)
                .padding(.top, m.topPadding)
                
                // --- Middle: Sliders (8 fixed columns) ---
                LazyVGrid(columns: m.columnsFixed, spacing: m.gridSpacing) {
                    VerticalSlider(label: "Freq", value: $frequency, range: 1...25,
                                   width: m.sliderWidth, height: m.sliderHeight, color: MixerPalette.channel[0])
                    VerticalSlider(label: "Speed", value: $speed, range: 0.1...5,
                                   width: m.sliderWidth, height: m.sliderHeight, color: MixerPalette.channel[1],
                                   stepCount: 12)
                    VerticalSlider(label: "Glow", value: $glow, range: 0.001...0.05,
                                   width: m.sliderWidth, height: m.sliderHeight, color: MixerPalette.channel[2])
                    VerticalSlider(label: "Shift", value: $colorShift, range: 0...3,
                                   width: m.sliderWidth, height: m.sliderHeight, color: MixerPalette.channel[3])
                    VerticalSlider(label: "Zoom", value: $zoom, range: 0.5...3,
                                   width: m.sliderWidth, height: m.sliderHeight, color: MixerPalette.channel[4])
                    VerticalSlider(label: "Iter", value: $iterations, range: 1...6,
                                   width: m.sliderWidth, height: m.sliderHeight, color: MixerPalette.channel[5])
                    VerticalSlider(label: "Hue", value: $hueShift, range: 0...2,
                                   width: m.sliderWidth, height: m.sliderHeight, color: MixerPalette.channel[6])
                    VerticalSlider(label: "Sym", value: $symmetry, range: 0.5...3,
                                   width: m.sliderWidth, height: m.sliderHeight, color: MixerPalette.channel[7])
                }
                .padding(.horizontal, m.horizontalPadding)
                .padding(.top, m.gapKnobToSlider)
                .frame(maxWidth: .infinity)
                .frame(height: m.sliderHeight + 56)
                
                // --- Bottom: Pads (8 fixed columns) ---
                LazyVGrid(columns: m.columnsFixed, spacing: 12) {
                    ForEach(0..<8, id: \.self) { i in
                        Button {
                            if padPressed.indices.contains(i) {
                                padPressed[i] = true
                                DispatchQueue.main.asyncAfter(deadline: .now() + 0.12) { padPressed[i] = false }
                            }
                            if i == 2 { exportShader() }
                            else if i == 3 { exportPNG() }
                            else if i == 4 { exportJPEG() }
                            else if i == 5 { exportGIF() }   // GIF
                        } label: { Color.clear }
                            .buttonStyle(PadButtonStyle(color: MixerPalette.channel[i],
                                                        size: m.padSize,
                                                        externallyPressed: padPressed[i]))
                            .frame(width: m.sliderWidth)
                    }
                }
                .padding(.horizontal, m.horizontalPadding)
                .padding(.top, m.gapSliderToPad)
                .padding(.bottom, m.bottomPadding)
            }
        }
        .frame(height: height)
    }
}

// MARK: - Square Pad Button Style
struct PadButtonStyle: ButtonStyle {
    var color: Color
    var size: CGFloat = 68
    var externallyPressed: Bool = false
    
    func makeBody(configuration: Configuration) -> some View {
        let down = configuration.isPressed || externallyPressed
        return configuration.label
            .frame(width: size, height: size)
            .background(
                RoundedRectangle(cornerRadius: 10, style: .continuous)
                    .fill(LinearGradient(colors: [color.opacity(down ? 0.92 : 1.0),
                                                  color.opacity(down ? 0.82 : 0.92)],
                                         startPoint: .top, endPoint: .bottom))
                    .overlay(
                        RoundedRectangle(cornerRadius: 10, style: .continuous)
                            .stroke(Color.white.opacity(0.16), lineWidth: 1)
                            .blendMode(.overlay)
                    )
                    .shadow(color: MixerPalette.edgeShadow, radius: down ? 2 : 6, x: 0, y: down ? 2 : 6)
            )
            .scaleEffect(down ? 0.98 : 1.0)
            .animation(.spring(response: 0.18, dampingFraction: 0.7), value: down)
    }
}

// MARK: - Rotary Knob
struct RotaryKnob: View {
    @Binding var progress: Double   // 0…1
    var diameter: CGFloat
    var color: Color
    
    private let startDeg = -140.0
    private let endDeg   =  140.0
    
    private var angle: Angle {
        .degrees(startDeg + (endDeg - startDeg) * Swift.min(1, Swift.max(0, progress)))
    }
    
    var body: some View {
        ZStack {
            Circle()
                .fill(LinearGradient(colors: [color.opacity(0.98), color.opacity(0.82)],
                                     startPoint: .top, endPoint: .bottom))
                .overlay(Circle().stroke(Color.white.opacity(0.18), lineWidth: 1).blendMode(.overlay))
                .shadow(color: .black.opacity(0.55), radius: 4, x: 0, y: 3)
            Circle()
                .fill(RadialGradient(colors: [Color.white.opacity(0.25), .clear],
                                     center: .topLeading,
                                     startRadius: diameter * 0.02,
                                     endRadius: diameter * 0.55))
            RoundedRectangle(cornerRadius: diameter * 0.03, style: .continuous)
                .fill(Color.white.opacity(0.95))
                .frame(width: diameter * 0.06, height: diameter * 0.40)
                .offset(y: -diameter * 0.18)
                .rotationEffect(angle)
        }
        .frame(width: diameter, height: diameter)
        .contentShape(Circle())
        .gesture(
            DragGesture(minimumDistance: 0, coordinateSpace: .local)
                .onChanged { g in
                    let center = CGPoint(x: diameter / 2, y: diameter / 2)
                    let dx = g.location.x - center.x
                    let dy = g.location.y - center.y
                    let rad = atan2(dx, -dy)
                    var deg = rad * 180.0 / .pi
                    deg = Swift.min(Swift.max(deg, startDeg), endDeg)
                    let t = (deg - startDeg) / (endDeg - startDeg)
                    progress = Swift.min(Swift.max(t, 0), 1)
                }
        )
        .accessibilityHidden(true)
    }
}

// MARK: - Mixer-Style Vertical Slider
struct VerticalSlider: View {
    var label: String
    @Binding var value: Float
    var range: ClosedRange<Float>
    var width: CGFloat
    var height: CGFloat
    var color: Color
    var stepCount: Int? = nil   // optional quantization
    
    @State private var isDragging = false
    
    var body: some View {
        VStack(spacing: 8) {
            ZStack {
                RoundedRectangle(cornerRadius: 12, style: .continuous)
                    .fill(MixerPalette.panel)
                    .overlay(
                        LinearGradient(
                            colors: [Color.white.opacity(0.10), .clear, .clear, Color.black.opacity(0.35)],
                            startPoint: .topLeading, endPoint: .bottomTrailing
                        )
                        .mask(RoundedRectangle(cornerRadius: 12, style: .continuous))
                    )
                    .shadow(color: Color.black.opacity(0.30), radius: 4, x: 0, y: 4)
                
                GeometryReader { proxy in
                    let W = proxy.size.width
                    let H = proxy.size.height
                    let topInset: CGFloat = 12
                    let bottomInset: CGFloat = 12
                    let handleH: CGFloat = 30
                    let handleW: CGFloat = min(W * 0.80, 48)
                    let slotW: CGFloat = min(14, W * 0.28)
                    
                    let minY = topInset
                    let maxY = H - bottomInset - handleH
                    let trackTopCenter = minY + handleH / 2
                    let trackBottomCenter = maxY + handleH / 2
                    let trackRange = trackBottomCenter - trackTopCenter
                    
                    // Value → position
                    let t = CGFloat((value - range.lowerBound) / (range.upperBound - range.lowerBound))
                    let yTop = (1 - t) * (maxY - minY) + minY
                    let faderX = (W - handleW) / 2
                    
                    // Track
                    RoundedRectangle(cornerRadius: slotW / 2, style: .continuous)
                        .fill(MixerPalette.slot)
                        .frame(width: slotW, height: H - topInset - bottomInset)
                        .position(x: W / 2, y: H / 2)
                        .overlay(RoundedRectangle(cornerRadius: slotW / 2).stroke(Color.white.opacity(0.08), lineWidth: 1))
                        .shadow(color: Color.black.opacity(0.8), radius: 4)
                    
                    // Tick marks (0,3,6,10 + small)
                    ForEach(0..<11, id: \.self) { i in
                        let isMajor = (i % 3 == 0) || i == 10
                        let tickLen: CGFloat = isMajor ? 10 : 6
                        let tickY = trackTopCenter + (1 - CGFloat(i) / 10.0) * trackRange
                        
                        Rectangle()
                            .fill(Color.white.opacity(isMajor ? 0.55 : 0.28))
                            .frame(width: tickLen, height: 1)
                            .position(x: (W / 2) - slotW / 2 - 6 - tickLen / 2, y: tickY)
                        
                        if [0, 3, 6, 10].contains(i) {
                            Text("\(i)")
                                .font(.system(size: 8, weight: .semibold, design: .rounded))
                                .foregroundColor(.white.opacity(0.7))
                                .position(x: (W / 2) - slotW / 2 - 18 - tickLen / 2, y: tickY - 5)
                        }
                    }
                    
                    // Fader handle (beveled cap)
                    FaderHandle(color: color, pressed: isDragging)
                        .frame(width: handleW, height: handleH)
                        .offset(x: faderX, y: yTop)
                        .shadow(color: .black.opacity(isDragging ? 0.25 : 0.45),
                                radius: isDragging ? 2 : 3, x: 0, y: isDragging ? 1 : 2)
                    
                    // Drag anywhere within column
                    Color.clear
                        .contentShape(Rectangle())
                        .gesture(
                            DragGesture(minimumDistance: 0)
                                .onChanged { g in
                                    let clamped = min(max(g.location.y, minY), maxY)
                                    var nt = 1 - (clamped - minY) / (maxY - minY)
                                    if let steps = stepCount, steps > 0 { nt = (round(nt * CGFloat(steps)) / CGFloat(steps)) }
                                    value = min(range.upperBound,
                                                max(range.lowerBound,
                                                    range.lowerBound + Float(nt) * (range.upperBound - range.lowerBound)))
                                    if !isDragging { isDragging = true }
                                }
                                .onEnded { _ in isDragging = false }
                        )
                }
                .padding(.horizontal, 8)
                .padding(.vertical, 6)
            }
            .frame(width: width, height: height)
            
            Text(label)
                .font(.caption.weight(.semibold))
                .foregroundColor(.white.opacity(0.9))
                .frame(width: width)
        }
    }
}

// MARK: - Fader Handle (colored cap)
struct FaderHandle: View {
    var color: Color
    var pressed: Bool
    var body: some View {
        ZStack {
            RoundedRectangle(cornerRadius: 6, style: .continuous)
                .fill(LinearGradient(colors: [color.opacity(pressed ? 0.95 : 1.0),
                                              color.opacity(pressed ? 0.85 : 0.92)],
                                     startPoint: .top, endPoint: .bottom))
                .overlay(
                    RoundedRectangle(cornerRadius: 6, style: .continuous)
                        .stroke(Color.white.opacity(0.18), lineWidth: 1)
                        .blendMode(.overlay)
                )
                .overlay(
                    LinearGradient(colors: [Color.white.opacity(0.20), .clear],
                                   startPoint: .top, endPoint: .center)
                    .clipShape(RoundedRectangle(cornerRadius: 6))
                )
            VStack {
                Spacer(minLength: 4)
                Capsule().fill(Color.white.opacity(0.35)).frame(height: 2).padding(.horizontal, 8)
                Spacer(minLength: 4)
                Capsule().fill(Color.white.opacity(0.35)).frame(height: 2).padding(.horizontal, 8)
                Spacer(minLength: 4)
            }
            Rectangle()
                .fill(
                    LinearGradient(
                        colors: [Color.white.opacity(0.55),
                                 Color.gray.opacity(0.20),
                                 Color.white.opacity(0.50)],
                        startPoint: .leading, endPoint: .trailing
                    )
                )
                .frame(height: 2)
                .padding(.horizontal, 6)
        }
    }
}

// MARK: - Metal Shader View & Renderer (live view)
struct MetalShaderView: UIViewRepresentable {
    var frequency, speed, glow, colorShift, zoom, iterations, hueShift, symmetry: Float
    var swirl, ripple, warp, twist, curlStrength, echo, flowAngle, cellNoise: Float
    @Binding var snapshot: UIImage?
    
    func makeCoordinator() -> Renderer {
        Renderer(frequency: frequency, speed: speed, glow: glow,
                 colorShift: colorShift, zoom: zoom, iterations: iterations,
                 hueShift: hueShift, symmetry: symmetry,
                 swirl: swirl, ripple: ripple, warp: warp, twist: twist,
                 curlStrength: curlStrength, echo: echo, flowAngle: flowAngle, cellNoise: cellNoise,
                 snapshot: $snapshot)
    }
    
    func makeUIView(context: Context) -> MTKView {
        let v = MTKView()
        v.device = MTLCreateSystemDefaultDevice()
        v.framebufferOnly = false
        v.isPaused = false
        v.enableSetNeedsDisplay = false
        v.preferredFramesPerSecond = 60
        v.delegate = context.coordinator
        return v
    }
    
    func updateUIView(_ uiView: MTKView, context: Context) {
        context.coordinator.updateParams(
            frequency: frequency, speed: speed, glow: glow,
            colorShift: colorShift, zoom: zoom, iterations: iterations,
            hueShift: hueShift, symmetry: symmetry,
            swirl: swirl, ripple: ripple, warp: warp, twist: twist,
            curlStrength: curlStrength, echo: echo, flowAngle: flowAngle, cellNoise: cellNoise
        )
    }
}

final class Renderer: NSObject, MTKViewDelegate {
    var device: MTLDevice!
    var pipeline: MTLRenderPipelineState!
    var queue: MTLCommandQueue!
    
    // Smooth speed integration for live view
    private var lastFrameTime = CACurrentMediaTime()
    private var accumulatedTime: Double = 0
    private var displaySpeed: Float
    
    @Binding var snapshot: UIImage?
    var frequency, speed, glow, colorShift, zoom, iterations, hueShift, symmetry: Float
    var swirl, ripple, warp, twist, curlStrength, echo, flowAngle, cellNoise: Float
    
    init(frequency: Float, speed: Float, glow: Float, colorShift: Float, zoom: Float,
         iterations: Float, hueShift: Float, symmetry: Float,
         swirl: Float, ripple: Float, warp: Float, twist: Float,
         curlStrength: Float, echo: Float, flowAngle: Float, cellNoise: Float,
         snapshot: Binding<UIImage?>) {
        self._snapshot = snapshot
        self.frequency = frequency; self.speed = speed; self.glow = glow
        self.colorShift = colorShift; self.zoom = zoom; self.iterations = iterations
        self.hueShift = hueShift; self.symmetry = symmetry
        self.swirl = swirl; self.ripple = ripple; self.warp = warp; self.twist = twist
        self.curlStrength = curlStrength; self.echo = echo; self.flowAngle = flowAngle; self.cellNoise = cellNoise
        self.displaySpeed = speed
        super.init()
        device = MTLCreateSystemDefaultDevice()
        buildPipeline()
    }
    
    func buildPipeline() {
        let src = """
        #include <metal_stdlib>
        using namespace metal;
        float2 rotate2D(float2 p, float a){float s=sin(a),c=cos(a);return float2(c*p.x - s*p.y, s*p.x + c*p.y);}
        float2 hash22(float2 p){p=float2(dot(p,float2(127.1,311.7)),dot(p,float2(269.5,183.3)));return fract(sin(p)*43758.5453);}
        float noise2d(float2 x){float2 i=floor(x);float2 f=fract(x);float2 u=f*f*(3.0-2.0*f);
            float2 a=hash22(i+float2(0.0,0.0));float2 b=hash22(i+float2(1.0,0.0));
            float2 c=hash22(i+float2(0.0,1.0));float2 d=hash22(i+float2(1.0,1.0));
            float n00=dot(a,f-float2(0.0,0.0));float n10=dot(b,f-float2(1.0,0.0));
            float n01=dot(c,f-float2(0.0,1.0));float n11=dot(d,f-float2(1.0,1.0));
            return mix(mix(n00,n10,u.x),mix(n01,n11,u.x),u.y);}
        float fbm(float2 p){float v=0.0,a=0.5;for(int i=0;i<4;++i){v+=a*noise2d(p);p*=2.0;a*=0.5;}return v;}
        float worley(float2 p){float2 ip=floor(p),fp=fract(p);float minD=10.0;
            for(int j=-1;j<=1;++j){for(int i=-1;i<=1;++i){float2 g=float2(i,j);
                float2 o=hash22(ip+g);float2 d=g+o-fp;minD=min(minD,length(d));}}return minD;}
        float2 gradWorley(float2 p){float e=0.005;float dx=worley(p+float2(e,0.0))-worley(p-float2(e,0.0));
            float dy=worley(p+float2(0.0,e))-worley(p-float2(0.0,e));return float2(dx,dy)/(2.0*e);}
        float2 gradFBM(float2 p){float e=0.005;float dx=fbm(p+float2(e,0.0))-fbm(p-float2(e,0.0));
            float dy=fbm(p+float2(0.0,e))-fbm(p-float2(0.0,e));return float2(dx,dy)/(2.0*e);}
        float2 curlField(float2 p){float2 g=gradFBM(p);return float2(g.y,-g.x);}
        float3 sceneCore(float2 uv,float time,float freq,float glow,float colorShift,float iter,float hueShift,float sym){
            float2 uv0=uv;float3 c=float3(0.0);
            for(float i=0.0;i<iter;i++){
                float2 u=fract(uv*sym)-0.5;
                float d=length(u)*exp(-length(uv0));
                float3 col=0.5+0.5*cos(float3(0.7,0.6,0.5)*(length(uv0)+colorShift+i+time*0.4+hueShift));
                d=sin(d*freq+time*1.2)/8.0; d=fabs(d); d=glow/d;
                c+=col*d; uv=u;
            }
            return c;
        }
        float2 preTransform(float2 uv,float t,float freq,float swirl,float ripple,float warpAmt,float twist){
            float r=length(uv);
            float2 dir=(r>1e-5)?uv/r:float2(0.0,1.0);
            float theta=atan2(uv.y,uv.x);
            float ang=swirl*r*r + twist*sin(3.0*theta)*r;
            uv=rotate2D(uv,ang);
            uv += dir*(sin(r*(6.0+freq*1.5)-t*3.0)*ripple);
            uv += warpAmt*float2(sin(uv.y*3.0 + t*1.3), cos(uv.x*3.0 - t*1.1));
            return uv;
        }
        struct VOut { float4 pos [[position]]; };
        vertex VOut vertexShader(uint vid [[vertex_id]]) {
            float2 pos[3] = { {-1.0,-1.0}, {3.0,-1.0}, {-1.0,3.0} };
            VOut o; o.pos=float4(pos[vid],0,1); return o;
        }
        fragment float4 fragmentShader(VOut in [[stage_in]],
          constant float2 &res         [[buffer(0)]],
          constant float &time         [[buffer(1)]],
          constant float &freq         [[buffer(2)]],
          constant float &glow         [[buffer(3)]],
          constant float &colorShift   [[buffer(4)]],
          constant float &zoom         [[buffer(5)]],
          constant float &iter         [[buffer(6)]],
          constant float &hueShift     [[buffer(7)]],
          constant float &sym          [[buffer(8)]],
          constant float &swirl        [[buffer(9)]],
          constant float &ripple       [[buffer(10)]],
          constant float &warpAmt      [[buffer(11)]],
          constant float &twist        [[buffer(12)]],
          constant float &curlStrength [[buffer(13)]],
          constant float &echoAmt      [[buffer(14)]],
          constant float &flowAngle    [[buffer(15)]],
          constant float &cellNoiseAmt [[buffer(16)]],
          constant float2 &noiseCurl   [[buffer(17)]],
          constant float2 &noiseCell   [[buffer(18)]]) {
          float2 uv = (in.pos.xy/res)*2.0 - 1.0;
          uv.x *= res.x/res.y;
          uv *= zoom;
          float t = time;
          if (curlStrength > 0.001) {
              float2 cv = curlField(uv * 1.4 + noiseCurl);
              uv += curlStrength * 0.15 * cv;
          }
          if (cellNoiseAmt > 0.001) {
              float2 gw = gradWorley(uv * 2.0 + noiseCell);
              float2 nrm = normalize(float2(gw.y, -gw.x) + 1e-6);
              uv += cellNoiseAmt * 0.12 * nrm;
          }
          float2 baseUV = preTransform(uv, t, freq, swirl, ripple, warpAmt, twist);
          float3 c = sceneCore(baseUV, t, freq, glow, colorShift, iter, hueShift, sym);
          if (echoAmt > 0.001) {
              float2 flowDir = float2(cos(flowAngle), sin(flowAngle));
              float stepLen = 0.05 + 0.18*echoAmt;
              float w1 = 0.45 * echoAmt;
              float w2 = 0.25 * echoAmt;
              float w3 = 0.15 * echoAmt;
              float norm = 1.0 + (w1 + w2 + w3);
              float t1 = t - 0.08, t2 = t - 0.16, t3 = t - 0.24;
              float2 uv1 = preTransform(uv - flowDir*stepLen,      t1, freq, swirl, ripple, warpAmt, twist);
              float2 uv2 = preTransform(uv - flowDir*stepLen*2.0,  t2, freq, swirl, ripple, warpAmt, twist);
              float2 uv3 = preTransform(uv - flowDir*stepLen*3.0,  t3, freq, swirl, ripple, warpAmt, twist);
              float3 c1 = sceneCore(uv1, t1, freq, glow, colorShift, iter, hueShift, sym);
              float3 c2 = sceneCore(uv2, t2, freq, glow, colorShift, iter, hueShift, sym);
              float3 c3 = sceneCore(uv3, t3, freq, glow, colorShift, iter, hueShift, sym);
              c = (c + c1*w1 + c2*w2 + c3*w3) / norm;
          }
          return float4(pow(c, float3(1.2)), 1.0);
        }
        """
        let lib = try! device.makeLibrary(source: src, options: nil)
        let desc = MTLRenderPipelineDescriptor()
        desc.vertexFunction = lib.makeFunction(name: "vertexShader")
        desc.fragmentFunction = lib.makeFunction(name: "fragmentShader")
        desc.colorAttachments[0].pixelFormat = .bgra8Unorm
        pipeline = try! device.makeRenderPipelineState(descriptor: desc)
        queue = device.makeCommandQueue()
    }
    
    func updateParams(frequency: Float, speed: Float, glow: Float,
                      colorShift: Float, zoom: Float, iterations: Float,
                      hueShift: Float, symmetry: Float,
                      swirl: Float, ripple: Float, warp: Float, twist: Float,
                      curlStrength: Float, echo: Float, flowAngle: Float, cellNoise: Float) {
        self.frequency=frequency; self.speed=speed; self.glow=glow
        self.colorShift=colorShift; self.zoom=zoom; self.iterations=iterations
        self.hueShift=hueShift; self.symmetry=symmetry
        self.swirl=swirl; self.ripple=ripple; self.warp=warp; self.twist=twist
        self.curlStrength=curlStrength; self.echo=echo; self.flowAngle=flowAngle; self.cellNoise=cellNoise
    }
    
    func draw(in view: MTKView) {
        guard let drawable = view.currentDrawable,
              let pass = view.currentRenderPassDescriptor else { return }
        
        let now = CACurrentMediaTime()
        let dt = max(0, now - lastFrameTime)
        lastFrameTime = now
        displaySpeed += (speed - displaySpeed) * 0.18
        accumulatedTime += dt * Double(displaySpeed)
        var t = Float(accumulatedTime)
        
        let cmd = queue.makeCommandBuffer()
        let enc = cmd?.makeRenderCommandEncoder(descriptor: pass)
        enc?.setRenderPipelineState(pipeline)
        
        var res = SIMD2<Float>(Float(view.drawableSize.width), Float(view.drawableSize.height))
        enc?.setFragmentBytes(&res, length: MemoryLayout<SIMD2<Float>>.stride, index: 0)
        enc?.setFragmentBytes(&t,   length: MemoryLayout<Float>.stride, index: 1)
        enc?.setFragmentBytes(&frequency,   length: 4, index: 2)
        enc?.setFragmentBytes(&glow,        length: 4, index: 3)
        enc?.setFragmentBytes(&colorShift,  length: 4, index: 4)
        enc?.setFragmentBytes(&zoom,        length: 4, index: 5)
        enc?.setFragmentBytes(&iterations,  length: 4, index: 6)
        enc?.setFragmentBytes(&hueShift,    length: 4, index: 7)
        enc?.setFragmentBytes(&symmetry,    length: 4, index: 8)
        enc?.setFragmentBytes(&swirl,       length: 4, index: 9)
        enc?.setFragmentBytes(&ripple,      length: 4, index: 10)
        enc?.setFragmentBytes(&warp,        length: 4, index: 11)
        enc?.setFragmentBytes(&twist,       length: 4, index: 12)
        enc?.setFragmentBytes(&curlStrength,length: 4, index: 13)
        enc?.setFragmentBytes(&echo,        length: 4, index: 14)
        enc?.setFragmentBytes(&flowAngle,   length: 4, index: 15)
        enc?.setFragmentBytes(&cellNoise,   length: 4, index: 16)
        
        // Live view: linear noise offsets (keeps current on-screen feel)
        var noiseCurl = SIMD2<Float>(t * 0.12, -t * 0.09)
        var noiseCell = SIMD2<Float>(t * 0.18,  t * 0.11)
        enc?.setFragmentBytes(&noiseCurl, length: MemoryLayout<SIMD2<Float>>.stride, index: 17)
        enc?.setFragmentBytes(&noiseCell, length: MemoryLayout<SIMD2<Float>>.stride, index: 18)
        
        enc?.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: 3)
        enc?.endEncoding()
        cmd?.present(drawable)
        cmd?.commit()
        
        snapshot = capture(drawable.texture)
    }
    
    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {}
    
    private func capture(_ tex: MTLTexture) -> UIImage? {
        let w = tex.width, h = tex.height
        var bytes = [UInt8](repeating: 0, count: w * h * 4)
        tex.getBytes(&bytes, bytesPerRow: w * 4, from: MTLRegionMake2D(0, 0, w, h), mipmapLevel: 0)
        guard let data = CFDataCreate(nil, bytes, w * h * 4) else { return nil }
        guard let prov = CGDataProvider(data: data) else { return nil }
        let cs = CGColorSpaceCreateDeviceRGB()
        guard let cg = CGImage(width: w, height: h, bitsPerComponent: 8, bitsPerPixel: 32, bytesPerRow: w * 4,
                               space: cs, bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedLast.rawValue),
                               provider: prov, decode: nil, shouldInterpolate: true, intent: .defaultIntent) else { return nil }
        return UIImage(cgImage: cg)
    }
}

// MARK: - GIF Export offscreen renderer (file-scope)
private struct ExportConfig {
    let frequency, speed, glow, colorShift, zoom, iterations, hueShift, symmetry: Float
    let swirl, ripple, warp, twist, curlStrength, echo, flowAngle, cellNoise: Float
}

private final class OffscreenRenderer {
    private let device: MTLDevice
    private let pipeline: MTLRenderPipelineState
    private let queue: MTLCommandQueue
    
    init?() {
        guard let dev = MTLCreateSystemDefaultDevice(),
              let q = dev.makeCommandQueue() else { return nil }
        device = dev
        queue = q
        
        let src = """
        #include <metal_stdlib>
        using namespace metal;
        float2 rotate2D(float2 p, float a){float s=sin(a),c=cos(a);return float2(c*p.x - s*p.y, s*p.x + c*p.y);}
        float2 hash22(float2 p){p=float2(dot(p,float2(127.1,311.7)),dot(p,float2(269.5,183.3)));return fract(sin(p)*43758.5453);}
        float noise2d(float2 x){float2 i=floor(x);float2 f=fract(x);float2 u=f*f*(3.0-2.0*f);
            float2 a=hash22(i+float2(0.0,0.0));float2 b=hash22(i+float2(1.0,0.0));
            float2 c=hash22(i+float2(0.0,1.0));float2 d=hash22(i+float2(1.0,1.0));
            float n00=dot(a,f-float2(0.0,0.0));float n10=dot(b,f-float2(1.0,0.0));
            float n01=dot(c,f-float2(0.0,1.0));float n11=dot(d,f-float2(1.0,1.0));
            return mix(mix(n00,n10,u.x),mix(n01,n11,u.x),u.y);}
        float fbm(float2 p){float v=0.0,a=0.5;for(int i=0;i<4;++i){v+=a*noise2d(p);p*=2.0;a*=0.5;}return v;}
        float worley(float2 p){float2 ip=floor(p),fp=fract(p);float minD=10.0;
            for(int j=-1;j<=1;++j){for(int i=-1;i<=1;++i){float2 g=float2(i,j);
                float2 o=hash22(ip+g);float2 d=g+o-fp;minD=min(minD,length(d));}}return minD;}
        float2 gradWorley(float2 p){float e=0.005;float dx=worley(p+float2(e,0.0))-worley(p-float2(e,0.0));
            float dy=worley(p+float2(0.0,e))-worley(p-float2(0.0,e));return float2(dx,dy)/(2.0*e);}
        float2 gradFBM(float2 p){float e=0.005;float dx=fbm(p+float2(e,0.0))-fbm(p-float2(e,0.0));
            float dy=fbm(p+float2(0.0,e))-fbm(p-float2(0.0,0.0));return float2(dx,dy)/(2.0*e);}
        float2 curlField(float2 p){float2 g=gradFBM(p);return float2(g.y,-g.x);}
        float3 sceneCore(float2 uv,float time,float freq,float glow,float colorShift,float iter,float hueShift,float sym){
            float2 uv0=uv;float3 c=float3(0.0);
            for(float i=0.0;i<iter;i++){
                float2 u=fract(uv*sym)-0.5;
                float d=length(u)*exp(-length(uv0));
                float3 col=0.5+0.5*cos(float3(0.7,0.6,0.5)*(length(uv0)+colorShift+i+time*0.4+hueShift));
                d=sin(d*freq+time*1.2)/8.0; d=fabs(d); d=glow/d;
                c+=col*d; uv=u;
            }
            return c;
        }
        float2 preTransform(float2 uv,float t,float freq,float swirl,float ripple,float warpAmt,float twist){
            float r=length(uv);
            float2 dir=(r>1e-5)?uv/r:float2(0.0,1.0);
            float theta=atan2(uv.y,uv.x);
            float ang=swirl*r*r + twist*sin(3.0*theta)*r;
            uv=rotate2D(uv,ang);
            uv += dir*(sin(r*(6.0+freq*1.5)-t*3.0)*ripple);
            uv += warpAmt*float2(sin(uv.y*3.0 + t*1.3), cos(uv.x*3.0 - t*1.1));
            return uv;
        }
        struct VOut { float4 pos [[position]]; };
        vertex VOut vertexShader(uint vid [[vertex_id]]) {
            float2 pos[3] = { {-1.0,-1.0}, {3.0,-1.0}, {-1.0,3.0} };
            VOut o; o.pos=float4(pos[vid],0,1); return o;
        }
        fragment float4 fragmentShader(VOut in [[stage_in]],
          constant float2 &res         [[buffer(0)]],
          constant float &time         [[buffer(1)]],
          constant float &freq         [[buffer(2)]],
          constant float &glow         [[buffer(3)]],
          constant float &colorShift   [[buffer(4)]],
          constant float &zoom         [[buffer(5)]],
          constant float &iter         [[buffer(6)]],
          constant float &hueShift     [[buffer(7)]],
          constant float &sym          [[buffer(8)]],
          constant float &swirl        [[buffer(9)]],
          constant float &ripple       [[buffer(10)]],
          constant float &warpAmt      [[buffer(11)]],
          constant float &twist        [[buffer(12)]],
          constant float &curlStrength [[buffer(13)]],
          constant float &echoAmt      [[buffer(14)]],
          constant float &flowAngle    [[buffer(15)]],
          constant float &cellNoiseAmt [[buffer(16)]],
          constant float2 &noiseCurl   [[buffer(17)]],
          constant float2 &noiseCell   [[buffer(18)]]) {
          float2 uv = (in.pos.xy/res)*2.0 - 1.0;
          uv.x *= res.x/res.y;
          uv *= zoom;
          float t = time;
          if (curlStrength > 0.001) {
              float2 cv = curlField(uv * 1.4 + noiseCurl);
              uv += curlStrength * 0.15 * cv;
          }
          if (cellNoiseAmt > 0.001) {
              float2 gw = gradWorley(uv * 2.0 + noiseCell);
              float2 nrm = normalize(float2(gw.y, -gw.x) + 1e-6);
              uv += cellNoiseAmt * 0.12 * nrm;
          }
          float2 baseUV = preTransform(uv, t, freq, swirl, ripple, warpAmt, twist);
          float3 c = sceneCore(baseUV, t, freq, glow, colorShift, iter, hueShift, sym);
          if (echoAmt > 0.001) {
              float2 flowDir = float2(cos(flowAngle), sin(flowAngle));
              float stepLen = 0.05 + 0.18*echoAmt;
              float w1 = 0.45 * echoAmt;
              float w2 = 0.25 * echoAmt;
              float w3 = 0.15 * echoAmt;
              float norm = 1.0 + (w1 + w2 + w3);
              float t1 = t - 0.08, t2 = t - 0.16, t3 = t - 0.24;
              float2 uv1 = preTransform(uv - flowDir*stepLen,      t1, freq, swirl, ripple, warpAmt, twist);
              float2 uv2 = preTransform(uv - flowDir*stepLen*2.0,  t2, freq, swirl, ripple, warpAmt, twist);
              float2 uv3 = preTransform(uv - flowDir*stepLen*3.0,  t3, freq, swirl, ripple, warpAmt, twist);
              float3 c1 = sceneCore(uv1, t1, freq, glow, colorShift, iter, hueShift, sym);
              float3 c2 = sceneCore(uv2, t2, freq, glow, colorShift, iter, hueShift, sym);
              float3 c3 = sceneCore(uv3, t3, freq, glow, colorShift, iter, hueShift, sym);
              c = (c + c1*w1 + c2*w2 + c3*w3) / norm;
          }
          return float4(pow(c, float3(1.2)), 1.0);
        }
        """
        guard let lib = try? device.makeLibrary(source: src, options: nil) else { return nil }
        let desc = MTLRenderPipelineDescriptor()
        desc.vertexFunction = lib.makeFunction(name: "vertexShader")
        desc.fragmentFunction = lib.makeFunction(name: "fragmentShader")
        desc.colorAttachments[0].pixelFormat = .bgra8Unorm
        guard let pipeline = try? device.makeRenderPipelineState(descriptor: desc) else { return nil }
        self.pipeline = pipeline
    }
    
    // Convenience: build CGImage from raw BGRA8 bytes
    static func imageFromBytes(_ bytes: [UInt8], width: Int, height: Int) -> CGImage? {
        guard let data = CFDataCreate(nil, bytes, bytes.count),
              let prov = CGDataProvider(data: data) else { return nil }
        let cs = CGColorSpaceCreateDeviceRGB()
        return CGImage(width: width, height: height, bitsPerComponent: 8, bitsPerPixel: 32,
                       bytesPerRow: width * 4, space: cs,
                       bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedLast.rawValue),
                       provider: prov, decode: nil, shouldInterpolate: true, intent: .defaultIntent)
    }
    
    // Render into a CGImage (single sample)
    func render(size: Int,
                time t: Float,
                params: ExportConfig,
                noiseCurl: SIMD2<Float>,
                noiseCell: SIMD2<Float>) -> CGImage? {
        guard let bytes = renderBytes(size: size, time: t, params: params,
                                      noiseCurl: noiseCurl, noiseCell: noiseCell) else { return nil }
        return Self.imageFromBytes(bytes, width: size, height: size)
    }
    
    // Render into BGRA bytes (used for temporal supersampling)
    func renderBytes(size: Int,
                     time t: Float,
                     params: ExportConfig,
                     noiseCurl: SIMD2<Float>,
                     noiseCell: SIMD2<Float>) -> [UInt8]? {
        let texDesc = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .bgra8Unorm,
                                                               width: size, height: size,
                                                               mipmapped: false)
        texDesc.usage = [.renderTarget, .shaderRead]
        texDesc.storageMode = .shared
        guard let texture = device.makeTexture(descriptor: texDesc),
              let cmd = (queue.makeCommandBuffer()) else { return nil }
        
        let pass = MTLRenderPassDescriptor()
        pass.colorAttachments[0].texture = texture
        pass.colorAttachments[0].loadAction = .clear
        pass.colorAttachments[0].storeAction = .store
        pass.colorAttachments[0].clearColor = MTLClearColorMake(0, 0, 0, 1)
        
        guard let enc = cmd.makeRenderCommandEncoder(descriptor: pass) else { return nil }
        enc.setRenderPipelineState(pipeline)
        
        var res = SIMD2<Float>(Float(size), Float(size))
        var time = t
        var f = params.frequency, g = params.glow, cs = params.colorShift
        var z = params.zoom, it = params.iterations, hueS = params.hueShift, sy = params.symmetry
        var sw = params.swirl, rp = params.ripple, wp = params.warp, tw = params.twist
        var curl = params.curlStrength, ec = params.echo, fa = params.flowAngle, cell = params.cellNoise
        var nc = noiseCurl, ncell = noiseCell
        
        enc.setFragmentBytes(&res,   length: MemoryLayout<SIMD2<Float>>.stride, index: 0)
        enc.setFragmentBytes(&time,  length: MemoryLayout<Float>.stride,          index: 1)
        enc.setFragmentBytes(&f,     length: 4, index: 2)
        enc.setFragmentBytes(&g,     length: 4, index: 3)
        enc.setFragmentBytes(&cs,    length: 4, index: 4)
        enc.setFragmentBytes(&z,     length: 4, index: 5)
        enc.setFragmentBytes(&it,    length: 4, index: 6)
        enc.setFragmentBytes(&hueS,  length: 4, index: 7)
        enc.setFragmentBytes(&sy,    length: 4, index: 8)
        enc.setFragmentBytes(&sw,    length: 4, index: 9)
        enc.setFragmentBytes(&rp,    length: 4, index: 10)
        enc.setFragmentBytes(&wp,    length: 4, index: 11)
        enc.setFragmentBytes(&tw,    length: 4, index: 12)
        enc.setFragmentBytes(&curl,  length: 4, index: 13)
        enc.setFragmentBytes(&ec,    length: 4, index: 14)
        enc.setFragmentBytes(&fa,    length: 4, index: 15)
        enc.setFragmentBytes(&cell,  length: 4, index: 16)
        enc.setFragmentBytes(&nc,    length: MemoryLayout<SIMD2<Float>>.stride, index: 17)
        enc.setFragmentBytes(&ncell, length: MemoryLayout<SIMD2<Float>>.stride, index: 18)
        
        enc.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: 3)
        enc.endEncoding()
        cmd.commit()
        cmd.waitUntilCompleted()
        
        // Read back
        var bytes = [UInt8](repeating: 0, count: size * size * 4)
        texture.getBytes(&bytes, bytesPerRow: size * 4,
                         from: MTLRegionMake2D(0, 0, size, size), mipmapLevel: 0)
        return bytes
    }
}

// MARK: - Share Sheet
struct ShareSheet: UIViewControllerRepresentable {
    var activityItems: [Any]
    func makeUIViewController(context: Context) -> UIActivityViewController {
        UIActivityViewController(activityItems: activityItems, applicationActivities: nil)
    }
    func updateUIViewController(_ uiViewController: UIActivityViewController, context: Context) {}
}

// MARK: - MIDI Manager (Korg nanoKONTROL2)
final class MIDIManager: ObservableObject {
    private var client = MIDIClientRef()
    private var inPort = MIDIPortRef()
    var onControlChange: ((UInt8, UInt8) -> Void)?
    
    func start() {
        MIDIClientCreate("ShaderMIDI" as CFString, nil, nil, &client)
        MIDIInputPortCreateWithBlock(client, "Input" as CFString, &inPort) { [weak self] packetList, _ in
            self?.parseMIDIPackets(packetList)
        }
        let count = MIDIGetNumberOfSources()
        for i in 0..<count {
            MIDIPortConnectSource(inPort, MIDIGetSource(i), nil)
        }
    }
    
    func stop() {
        if inPort != 0 { MIDIPortDispose(inPort); inPort = 0 }
        if client != 0 { MIDIClientDispose(client); client = 0 }
    }
    
    private func parseMIDIPackets(_ packetList: UnsafePointer<MIDIPacketList>) {
        var packet = packetList.pointee.packet
        for _ in 0..<packetList.pointee.numPackets {
            let length = Int(packet.length)
            let data = Mirror(reflecting: packet.data).children.prefix(length).compactMap { $0.value as? UInt8 }
            var i = 0
            while i + 2 < data.count {
                let status = data[i]
                if (status & 0xF0) == 0xB0 {           // Control Change
                    let cc = data[i + 1]
                    let val = data[i + 2]
                    onControlChange?(cc, val)
                    i += 3
                } else { i += 1 }
            }
            packet = MIDIPacketNext(&packet).pointee
        }
    }
}
