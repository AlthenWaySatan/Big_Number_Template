#include<iostream>
#include<cstdlib>
#include<cstring>
#include<cstdio>
#include<string>
#include<cmath>
using namespace std;
const double pi=acos(-1.0);
const int Longnum_max_lenth=110;
struct Longnum{
    int len,a[Longnum_max_lenth],cas;
    bool read(){
        memset(a,0,sizeof(a));
        int t[Longnum_max_lenth];
        char ch;bool ret=0;ch=getchar();cas=1;len=0;
        while(ch<'0'||ch>'9'){if(ch=='-')cas=-1;ch=getchar();}
        while(ch>='0'&&ch<='9'){ret=1;t[++len]=(ch&15);ch=getchar();}
        for(int i=1;i<=len;i++)a[i]=t[len-i+1];
        while(a[len]==0&&len>=1)len--;if(!len)cas=0;
        return ret;
    }
    
	Longnum &operator =(long long v){
	  memset(a,0,sizeof(a));
		len=0;cas=1;
	  if(v<0)cas=-1,v=-v;
	  if(v==0)cas=0;
	  while(v){a[++len]=v%10;v/=10;}
	}
    void print(){
        if(len==0){cout<<0;return;}
        if(cas==-1)cout<<'-';
        for(int i=len;i>=1;i--)cout<<a[i];
    }
    long long derive(){
        long long x=0ll;
        for(int i=len;i>=1;i--)x=(x<<1)+(x<<3)+(1ll*a[i]);
        return x*cas;
    }
};

ostream& operator <<(ostream& os,const Longnum &x){
    if(x.len==0){os<<0;return os;}
    if(x.cas==-1)os<<'-';
    for(int i=x.len;i>=1;i--)os<<x.a[i];
    return os;
}

istream& operator >>(istream& is,Longnum &x){
    memset(x.a,0,sizeof(x.a));
    int *t=new int[Longnum_max_lenth];
    char *ch= new char[Longnum_max_lenth+1];
    is>>ch;
    x.cas=1;x.len=0;
    for(int i=0;ch[i];i++){
        if(ch[i]=='-')x.cas=-1;
        if(ch[i]>='0'&&ch[i]<='9')t[++x.len]=(ch[i]&15);
    }
    for(int i=1;i<=x.len;i++)x.a[i]=t[x.len-i+1];
    while(x.a[x.len]==0&&x.len>=1)x.len--;if(!x.len)x.cas=0;
    delete []t;
    delete []ch;
    return is;
}

inline bool operator <(const Longnum &a,const Longnum &b){
    if(a.cas!=b.cas)return a.cas<b.cas;bool ret=(a.cas==-1);
    if(a.len!=b.len)return (a.len<b.len)^ret;
    for(int i=a.len;i>1;i--)if(a.a[i]!=b.a[i])return (a.a[i]<b.a[i])^ret;
    return (a.a[1]<b.a[1])^ret;
}
inline bool operator >(const Longnum &a,const Longnum &b){
    if(a.cas!=b.cas)return a.cas>b.cas;bool ret=(a.cas==-1);
    if(a.len!=b.len)return (a.len>b.len)^ret;
    for(int i=a.len;i>1;i--)if(a.a[i]!=b.a[i])return (a.a[i]>b.a[i])^ret;
    return (a.a[1]>b.a[1])^ret;
}
inline bool operator ==(const Longnum &a,const Longnum &b){
    if(a.cas!=b.cas)return 0;
    if(a.len!=b.len)return 0;
    for(int i=a.len;i>1;i--)if(a.a[i]!=b.a[i])return 0;
    return a.a[1]==b.a[1];
}
inline bool operator <=(const Longnum &a,const Longnum &b){return a<b||a==b;}
inline bool operator >=(const Longnum &a,const Longnum &b){return a>b||a==b;}
inline bool operator !=(const Longnum &a,const Longnum &b){return !(a==b);}

inline Longnum max(Longnum a,Longnum b){if(a>b)return a;else return b;}
inline Longnum min(Longnum a,Longnum b){if(a<b)return a;else return b;}
inline Longnum abs(Longnum a){if(a.cas=-1)a.cas=1;return a;}

inline Longnum merge_add(Longnum a,Longnum b){
    Longnum Ret;Ret.len=max(a.len,b.len);
    memset(Ret.a,0,sizeof(Ret.a));
    for(int i=1;i<=Ret.len;i++){Ret.a[i]+=(a.a[i]+b.a[i]);Ret.a[i+1]+=Ret.a[i]/10;Ret.a[i]%=10;}
    if(Ret.a[Ret.len+1])Ret.len++;
    return Ret;
}
inline Longnum merge_sub(Longnum a,Longnum b){
    Longnum Ret=a;
    for(int i=1;i<=a.len;i++){Ret.a[i]-=b.a[i];if(Ret.a[i]<0)Ret.a[i]+=10,Ret.a[i+1]--;}
    while(Ret.a[Ret.len]==0&&Ret.len>=1)Ret.len--;
    return Ret;
}
inline Longnum operator +(Longnum a,Longnum b){
    Longnum Ret;
    if(a.cas==b.cas){Ret=merge_add(a,b);Ret.cas=a.cas;if(!Ret.len)Ret.cas=0;return Ret;}
    if(!a.cas)return b;if(!b.cas)return a;
    if(a.cas==-1){a.cas=1;
        if(a>b){Ret=merge_sub(a,b);Ret.cas=-1;if(!Ret.len)Ret.cas=0;return Ret;}
        else {Ret=merge_sub(b,a);Ret.cas=1;if(!Ret.len)Ret.cas=0;return Ret;}
    }
    if(b.cas==-1){b.cas=1;
        if(b>a){Ret=merge_sub(b,a);Ret.cas=-1;if(!Ret.len)Ret.cas=0;return Ret;}
        else {Ret=merge_sub(a,b);Ret.cas=1;if(!Ret.len)Ret.cas=0;return Ret;}
    }
}
inline Longnum operator -(Longnum a,Longnum b){
    Longnum Ret;
    if(a.cas!=b.cas&&a.cas&&b.cas){Ret=merge_add(a,b);Ret.cas=a.cas;if(!Ret.len)Ret.cas=0;return Ret;}
    if(!a.cas){b.cas*=-1;return b;}
    if(!b.cas)return a;
    if(a.cas==1){
        if(a>b){Ret=merge_sub(a,b);Ret.cas=1;if(!Ret.len)Ret.cas=0;return Ret;}
        else {Ret=merge_sub(b,a);Ret.cas=-1;if(!Ret.len)Ret.cas=0;return Ret;}
    }
    if(a.cas==-1){
        if(a<b){Ret=merge_sub(a,b);Ret.cas=-1;if(!Ret.len)Ret.cas=0;return Ret;}
        else {Ret=merge_sub(b,a);Ret.cas=1;if(!Ret.len)Ret.cas=0;return Ret;}
    }
}
inline Longnum operator +=(Longnum &a,Longnum b){a=a+b;}
inline Longnum operator -=(Longnum &a,Longnum b){a=a-b;}

struct cpx{
  double r,i;
  inline cpx operator *(const cpx&x)const{return (cpx){r*x.r-i*x.i,r*x.i+i*x.r};}
  inline cpx operator +(const cpx&x)const{return (cpx){r+x.r,i+x.i};}
  inline cpx operator -(const cpx&x)const{return (cpx){r-x.r,i-x.i};}
}cpxa[Longnum_max_lenth],cpxb[Longnum_max_lenth];
int R[Longnum_max_lenth];
void FFT(cpx*a,int f,int la){
  int n=la;
  for(register int i=0;i<n;++i)if(i<R[i])swap(a[i],a[R[i]]);
  for(register int i=1;i<n;i<<=1){
    cpx wn=(cpx){cos(pi/i),f*sin(pi/i)};
    for(register int j=0;j<n;j+=(i<<1)){
      cpx w=(cpx){1,0};
      for(register int k=0;k<i;++k,w=w*wn){
          cpx x=a[j+k],y=w*a[j+k+i];
          a[j+k]=x+y;a[j+k+i]=x-y;
      }
    }
  }
  if(f==-1)
  for(register int i=0;i<n;i++)a[i].r/=n;
}
int merge_fft(cpx *a,cpx *b,int la,int lb){
  int n=la,m=lb;
  int L=0;for(m+=n,n=1;n<=m;n<<=1)L++;
  for(register int i=0;i<n;i++)
  R[i]=(R[i>>1]>>1)|((i&1)<<(L-1));
  FFT(a,1,n);FFT(b,1,n);
  for(register int i=0;i<=n;i++)a[i]=a[i]*b[i];
  FFT(a,-1,n);
  return m;
}

inline Longnum operator *(const Longnum &a,const Longnum &b){
    memset(R,0,sizeof(R));
    memset(cpxa,0,sizeof(cpxa));
    memset(cpxb,0,sizeof(cpxb));
    Longnum Ret;
    memset(Ret.a,0,sizeof(Ret.a));
    Ret.cas=a.cas*b.cas;
    for(register int i=0;i<=a.len;i++)cpxa[i].r=a.a[i];
  for(register int i=0;i<=b.len;i++)cpxb[i].r=b.a[i];
  Ret.len=merge_fft(cpxa,cpxb,a.len,b.len)-1;
  for(register int i=1;i<=Ret.len;i++)
    {Ret.a[i]+=(int)(cpxa[i+1].r+0.1);Ret.a[i+1]+=Ret.a[i]/10;Ret.a[i]%=10;}
  if(Ret.a[Ret.len+1])Ret.len++;
  while(Ret.a[Ret.len]==0&&Ret.len>=1)Ret.len--;
  return Ret;
}
inline Longnum operator *=(Longnum &a,Longnum b){a=a*b;}
inline void merge_mid(Longnum &a){
  for(int i=a.len;i>=1;i--)a.a[i-1]+=(a.a[i]&1)*10,a.a[i]/=2;a.a[0]=0;
  while(a.a[a.len]==0&&a.len>=1)a.len--;if(!a.len)a.cas=0;
}
Longnum for_ls,for_a,for_b,for_l,for_r,for_mid,Num_1;
inline Longnum operator /(Longnum &a,Longnum &b){
	Num_1=1;
	for_a=a;for_b=b;int flag=1;
	for_a.cas*=for_b.cas;for_b.cas*=for_b.cas;
	flag=for_a.cas;for_a.cas*=for_a.cas;
	for_l=0;for_r=a;
	while(for_l<for_r){
	  for_mid=for_l+for_r;
		for_mid=for_mid+Num_1;
	  merge_mid(for_mid);
	  if(for_mid*for_b>for_a)for_r=for_mid-Num_1;
	  else for_l=for_mid;
	}
	for_l.cas*=flag;return for_l;
}
inline Longnum operator /=(Longnum &a,Longnum b){a=a/b;}
inline Longnum operator %(Longnum &a,Longnum &b){Longnum g=a/b;g*=b;return a-g;}
inline Longnum operator %=(Longnum &a,Longnum b){a=a%b;}
Longnum a,b,c; 
int main()
{
	int Size = 32<<20;
  char *X = (char*)malloc(Size) + Size;
  __asm__("movl %0, %%esp\n" :: "r"(X));
  a=123456123,b=1000;c=a%b;
  cout<<c<<endl;
	return 0;
}

