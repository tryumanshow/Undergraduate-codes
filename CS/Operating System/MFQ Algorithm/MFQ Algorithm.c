#define _CRT_SECURE_NO_WARNINGS

#define max_process_per_queue 100 // [가정] 각 큐마다 최대로 담길 수 있는 process의 수: 100개
#define max_process 100 // [가정] 입력으로 받을 수 있는 process의 수: 100개
#define max_computing_time 10000
#define total_queue 3 // 전체 큐의 개수 (문제의 조건) 
#define q0 0 // Q0
#define q1 1 // Q1
#define q2 2 // Q2

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// 각 process 마다의 정보를 저장하는 구조체
typedef struct process_info {
	int process_num; // 입력받는 값 (process의 번호)
	int arrival_time, burst_time; // 입력받는 값 
	int turnaround_time, waiting_time, start_time, end_time; // 계산해 낼 값
	int run_time; 
} process;

// 각 process를 저장한 구조체들의 배열
process pc_address[max_process]; 

int process_in_cpu[max_computing_time]; // cpu 내에서 실행 중인 process의 번호를 삽입한 배열 

// 각 queue에 process들을 담을 것
typedef struct queue {
	process *queue[max_process_per_queue]; // 구조체 내 포인터 선언.
	int first, last, length;
} queue;

// 큐 초기화
void init_queue(queue *q){ 
	int i;

	for (i=0; i<max_process_per_queue; i++){
		(*q).queue[i] = NULL;
	}
	
	(*q).first = 0;
	(*q).last = 0;
	(*q).length = 0;
}

// 빈 큐인지 확인
int queue_isempty(queue *q){
	if(q -> length == 0) return 1; // Queue: Empty
	else return 0; // Queue: Not Empty 
}

// 큐에 Push
void push_to_queue(queue *q, process *p){

	int i, j, min, index;
	process *temp = NULL;

	(*q).queue[q -> last] = p;
	(*q).last += 1;
	(*q).length += 1;

	// push한 이후에는 미리 arrival time 순으로 queue 내의 모든 process 정렬
	// 선택정렬 알고리즘 사용 
	for(i=(*q).first; i<(*q).last;i++){
		min=9999;
		for(j=i; j<(*q).last; j++){
			if(min > q->queue[j]->arrival_time){
				min = q->queue[j]->arrival_time;
				index = j;
			}
		}
		temp = (*q).queue[i];
		(*q).queue[i] = (*q).queue[index];
		(*q).queue[index] = temp;
	}
}

// 큐로부터 Pop
process *pop_from_queue(queue *q, int cpu_time){

	process *pop = NULL;

	if(queue_isempty(q)==0){
		if((*q).queue[(*q).first]->arrival_time <= cpu_time){
			pop = (*q).queue[(*q).first];
			(*q).first++;
			(*q).length--;
		}
	}
	return pop;
}

int in_progress(process *p, int cpu_time){

	(*p).run_time++;

	// 처음 들어왔다면 당시의 cpu_time을 start_time을 할당. 
	if((*p).start_time == -999){
		(*p).start_time = cpu_time;
	}

	if((*p).burst_time == (*p).run_time){
		(*p).end_time = cpu_time;
		return 1;
	}
	else return 0;	
}

int mfq(int cnt, int cpu_time){

	queue ready_queue[total_queue];
	process *present_pcs=NULL;
	process *next_pcs=&pc_address[0];
	int total_execution_time=0;

	int num_of_finish = 0; // 각 프로세스가 완벽히 종료되면 +1
	int message=0; // q0로의 arrival_time과 cpu_time이 일치할 때만 1, 나머지 0
	int position=0; // 해당 cpu_time에서 가장 우선순위에 있는 큐 번호 (0,1,2)

	// 큐 생성
	init_queue(&ready_queue[q0]);
	init_queue(&ready_queue[q1]);
	init_queue(&ready_queue[q2]);

	// 간트 차트 그려주는 건, 일단 값들 다 얻어놓고 나중에 생각하자
	for(cpu_time; num_of_finish < cnt; cpu_time++){
		
		total_execution_time++;
		// arrival_time과 cpu_time이 같아지면 자동으로 q0으로 쏴주도록. 
		message = 0;
		if((*next_pcs).arrival_time == cpu_time){
			message = 1;
			push_to_queue(&ready_queue[q0], next_pcs);
			next_pcs++;
		}

		// 실행 중인 프로세스가 없을 경우
		if(present_pcs == NULL){
			position = 0;
			if(queue_isempty(&ready_queue[q0]) == 0){
				present_pcs = pop_from_queue(&ready_queue[q0], cpu_time);
				process_in_cpu[cpu_time] = present_pcs->process_num; // 추가
				position = q0;
			}

			else if (queue_isempty(&ready_queue[q0]) == 1 && queue_isempty(&ready_queue[q1]) == 0){
				present_pcs = pop_from_queue(&ready_queue[q1], cpu_time);
				process_in_cpu[cpu_time] = present_pcs->process_num; // 추가
				position = q1;
			}

			else{
				present_pcs = pop_from_queue(&ready_queue[q2], cpu_time);
				process_in_cpu[cpu_time] = present_pcs->process_num; // 추가
				position = q2;
			}
			
			cpu_time--;
			total_execution_time--;
		}
		
		else{
			// 두 번째 혹은 세 번째 큐에서 실행 중일 때, 다른 프로세스가 '막' 도착하는 경우
			if(position != q0 && message == 1){ 
				push_to_queue(&ready_queue[position], present_pcs);
				present_pcs = pop_from_queue(&ready_queue[q0], cpu_time);
				process_in_cpu[cpu_time] = present_pcs->process_num; // 추가
				position = q0;
			}

			if(in_progress(present_pcs, cpu_time) == 0){
				
				if((*present_pcs).run_time == 2){
					push_to_queue(&ready_queue[q1], present_pcs);
					process_in_cpu[cpu_time] = present_pcs->process_num;
					present_pcs = NULL;
				}

				else if((*present_pcs).run_time == 6){
					push_to_queue(&ready_queue[q2], present_pcs);
					process_in_cpu[cpu_time] = present_pcs->process_num;
					present_pcs = NULL;
				}

				else process_in_cpu[cpu_time] = present_pcs->process_num;
			}

			else{
				num_of_finish ++;
				process_in_cpu[cpu_time] = present_pcs->process_num;
				present_pcs = NULL;
			}
		}
	}
	return total_execution_time;
}


void result_table(int cnt){ // cnt: process cnt
	
	int i;
	int tt_sum=0, wt_sum=0;

	puts("+-----+------------+------------------+-----------------+");
	puts("| PID | Burst Time | Turnaround Time  |   Waiting Time  |");
	puts("+-----+------------+------------------+-----------------+");

	for(i=0; i<cnt; i++){
		printf("| %2d  |     %2d     |        %2d        |       %2d        |\n", 
			pc_address[i].process_num, pc_address[i].burst_time, pc_address[i].turnaround_time, pc_address[i].waiting_time);
	}
	printf("+-----+------------+------------------+-----------------+\n");
	printf("\n");

	for(i=0; i<cnt; i++){
		tt_sum += pc_address[i].turnaround_time;
		wt_sum += pc_address[i].waiting_time;
	}

	printf("전체 프로세스의 평균 TT: %d\n", tt_sum/cnt);
	printf("전체 프로세스의 평균 WT: %d\n", wt_sum/cnt);
	printf("\n");
}


void for_gantt_chart(int total){ 

	// 전부다 반복문을 돌리기 위한 임시변수일 뿐. 
	int i, j;
	int p=0, q, cnt;
	int hyphen_cnt=0, gantt_temp; // hyphen_cnt: 간트 차트의 총 세로 길이 gantt_temp: 완전히 종료되어 나오는 프로세스들 중 제일 마지막 프로세스의 숫자까지의 길이
	int temp1, temp2, temp3=0, length=0;

	puts(	"Gantt Chart"	);
	printf("+");
	for(i=0;i<total;i++){
		if(i<(total-1)) {
			printf("---");
			hyphen_cnt+=3;
		}
		if(i==(total-1)) {
			printf("--+\n");
			hyphen_cnt+=2;
		}
	}
	printf("|");
	
	while(p<total){
		cnt=1;
		for(q=p+1;q<total; q++){
			if(process_in_cpu[p]==process_in_cpu[q]) cnt++;
			else break;
		}
		for(temp1=0; temp1<cnt; temp1++){
			printf(" ");
		}
		if(cnt>4) printf(" ");

		printf("%d", process_in_cpu[p]);
		for(temp1=0; temp1<cnt; temp1++){
			printf(" ");
		}
		if(cnt>4) printf(" ");
		printf("|");
		p += cnt;
	}

	printf("\n");
	printf("+");
	for(i=0;i<total;i++){
		if(i<(total-1)) printf("---");
		if(i==(total-1)) printf("--+\n");
	}

	printf("   ");

	temp2 = 0;
	i = temp2;
	do{
		for(j=i+1; j<total; j++){
			if (process_in_cpu[i] == process_in_cpu[j]){
				temp2++;
			}
			else {
				length = temp2-i+1;
				temp2++;
				break;
			}
		}

		if(length>1){
			for(temp3=0; temp3<length; temp3++) {
				printf(" ");
				if (temp3 >= 3) printf(" ");
			}

		}
		else printf(" ");

		// 큐를 이동한 뒤에도, 동일한 프로세스가 연달아 CPU를 잡고 실행하게 될 때의 Gantt Chart 표기를 위한 예외 처리 

		if(temp2-i<=4 && process_in_cpu[temp2] != process_in_cpu[i]) printf("%2d", temp2);
		
		if(length>1){
			for(temp3=0; temp3<length; temp3++) printf(" ");
		}
		else printf(" ");

		i = temp2;
		gantt_temp = i;

		if (i==(total-1)) printf("%d\n", total);
		

	}while(i!=(total-1));
	
	printf("\n");
}

int main(void){

	int process_cnt; // process_cnt: 입력시 받은 총 process의 개수
	int i=-1;
	int k, l, temp;
	int fore, back, sign;
	int cpu_time = 0; // 1초 단위로 매 iteration마다 증가	
	int total = 0;

	FILE *fp = fopen("/My Directory","rt");

	if(fp==NULL){
		printf("파일오픈 실패\n");
	}
	
	else{
		while(!feof(fp))
		{
			if(i==-1){
				fscanf(fp, "%d", &process_cnt);
			}
			else {
				fscanf(fp, "%d %d %d", &pc_address[i].process_num, &pc_address[i].arrival_time, &pc_address[i].burst_time);
				pc_address[i].turnaround_time = 0;
				pc_address[i].waiting_time = 0;
				pc_address[i].start_time = -999; // 처음 들어온 process는 아직 시작x
				pc_address[i].end_time = 0;
				pc_address[i].run_time = 0;
			}
			++i;
		}
	}

	total += mfq(process_cnt, cpu_time);
	
	for(k=0; k<process_cnt; k++){
		temp = 0; 
		fore = 0; 
		back = 0; 
		sign = 0;
		for (l=0; l<total; l++){
			if(process_in_cpu[l] == (k+1)){
				if(sign==0){
					fore=temp;
					sign++;
				}
				else{
					back=temp+1;
				}
			}
			temp++;
		}
		pc_address[k].start_time = fore;
		pc_address[k].turnaround_time = back-pc_address[k].arrival_time;
		pc_address[k].waiting_time = pc_address[k].turnaround_time - pc_address[k].burst_time;
	}

	
	result_table(process_cnt);
	for_gantt_chart(total);

	system("pause");
	
	return 0;
}