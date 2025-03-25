# main.py
import os
import csv
import time
from datetime import datetime
import random
import math

try:
    from colorama import init, Fore, Back, Style
    COLORAMA_AVAILABLE = True
except ImportError:
    COLORAMA_AVAILABLE = False
    print("Dica: Instale a biblioteca 'colorama' para uma experiência visual melhorada!")
    print("Use: pip install colorama")

try:
    from tabulate import tabulate
    TABULATE_AVAILABLE = True
except ImportError:
    TABULATE_AVAILABLE = False
    print("Dica: Instale a biblioteca 'tabulate' para uma exibição em tabelas!")
    print("Use: pip install tabulate")

"""
FARMTECH SOLUTIONS - PROJETO AGRICULTURA DIGITAL
------------------------------------------------
Este código realiza o cálculo de área e insumos para duas culturas:
1) Cana-de-Açúcar - área considerada como retângulo (base x altura)
   - Cálculo real: considerando o espaçamento entre ruas, número estimado de ruas e taxa de aplicação de 0,088 L/m².
2) Mandioca - área considerada como retângulo (base x altura)
   - Utilizando uma taxa simbólica de 0,05 L/m² para insumo (fertilizante orgânico).

Funcionalidades:
- Inserção de dados (cálculo de área e insumo), armazenando em uma lista;
- Atualização de dados;
- Exclusão de dados do vetor;
- Exibição de dados;
- Exportação de dados para CSV (para análise em R);
- Encerramento da aplicação.
"""

# Inicializar colorama se disponível
if COLORAMA_AVAILABLE:
    init(autoreset=True)

# Lista para armazenar os registros de cada cultura
cultura_dados = []

def colored(text, color=None, background=None, style=None):
    """Função auxiliar para aplicar cores ao texto, caso o colorama esteja disponível."""
    if not COLORAMA_AVAILABLE:
        return text

    result = ""
    if color:
        result += getattr(Fore, color.upper())
    if background:
        result += getattr(Back, background.upper())
    if style:
        result += getattr(Style, style.upper())

    result += text + Style.RESET_ALL
    return result

def show_loading(message="Carregando", duration=1.0, width=40):
    """Mostra uma animação de carregamento no terminal."""
    if not COLORAMA_AVAILABLE:
        print(f"{message}...")
        time.sleep(duration)
        return

    steps = 20
    delay = duration / steps

    for i in range(steps):
        filled = math.floor((i / steps) * width)
        bar = colored("[" + "=" * filled + " " * (width - filled) + "]", color="cyan")
        print(f"\r{message} {bar} {(i+1)*5}%", end="")
        time.sleep(delay)

    print("\r" + " " * (len(message) + width + 10), end="\r")
    print(colored(f"{message} concluído!", color="green", style="bright"))

def clear_screen():
    """Limpa a tela do terminal (compatível com Windows e Linux)."""
    os.system('cls' if os.name == 'nt' else 'clear')

def print_ascii_banner():
    """Exibe um banner estilizado no topo do menu da aplicação."""
    if COLORAMA_AVAILABLE:
        banner = f"""
        {Fore.GREEN}╔══════════════════════════════════════════════════╗
        {Fore.GREEN}║  {Fore.YELLOW}FARMTECH SOLUTIONS {Fore.WHITE}- {Fore.CYAN}AGRICULTURA DIGITAL{Fore.GREEN}  ║
        {Fore.GREEN}║              {Fore.MAGENTA}Cultivo Inteligente{Fore.GREEN}               ║
        {Fore.GREEN}╚══════════════════════════════════════════════════╝{Style.RESET_ALL}
        """
    else:
        banner = r"""
         ================================================
        ||        FARMTECH SOLUTIONS - AGRICULTURA      ||
        ||                Cultivo Digital               ||
        =================================================
        """
    print(banner)

def input_float(prompt):
    """
    Solicita uma entrada numérica (float) ao usuário com validação.

    Parâmetros:
        prompt (str): Mensagem de solicitação.

    Retorna:
        float: Valor numérico inserido pelo usuário.
    """
    while True:
        valor_str = input(colored(prompt, color="cyan") if COLORAMA_AVAILABLE else prompt)
        try:
            valor = float(valor_str)
            if valor <= 0:
                print(colored("Por favor, insira um valor positivo maior que zero.", color="red")
                      if COLORAMA_AVAILABLE else "Por favor, insira um valor positivo maior que zero.")
                continue
            return valor
        except ValueError:
            print(colored("Valor inválido. Por favor, insira um número.", color="red")
                  if COLORAMA_AVAILABLE else "Valor inválido. Por favor, insira um número.")

# ------------------------- CÁLCULOS DE ÁREA ------------------------- #

def calcular_area_cana():
    """
    Realiza o cálculo da área para Cana-de-Açúcar considerando uma forma retangular.

    Solicita os valores de base e altura e retorna a área calculada.

    Retorna:
        float: Área calculada.
    """
    print(colored("\n=== Cálculo de Área para Cana de Açúcar (Retângulo) ===", color="yellow", style="bright")
          if COLORAMA_AVAILABLE else "\n=== Cálculo de Área para Cana de Açúcar (Retângulo) ===")

    base = input_float("Digite a base (m): ")
    altura = input_float("Digite a altura (m): ")

    print(colored("\nCalculando área...", color="cyan") if COLORAMA_AVAILABLE else "\nCalculando área...")
    time.sleep(0.5)

    area = base * altura
    resultado = f"Área de Cana de Açúcar: {area:.2f} m²"

    print(colored(resultado, color="green", style="bright") if COLORAMA_AVAILABLE else resultado)
    return area

def calcular_area_mandioca():
    """
    Realiza o cálculo da área para Mandioca considerando uma forma retangular.

    Solicita os valores de base e altura e retorna a área calculada.

    Retorna:
        float: Área calculada.
    """
    print(colored("\n=== Cálculo de Área para Mandioca (Retângulo) ===", color="yellow", style="bright")
          if COLORAMA_AVAILABLE else "\n=== Cálculo de Área para Mandioca (Retângulo) ===")

    base = input_float("Digite a base (m): ")
    altura = input_float("Digite a altura (m): ")

    print(colored("\nCalculando área...", color="cyan") if COLORAMA_AVAILABLE else "\nCalculando área...")
    time.sleep(0.5)

    area = base * altura
    resultado = f"Área de Mandioca: {area:.2f} m²"

    print(colored(resultado, color="green", style="bright") if COLORAMA_AVAILABLE else resultado)
    return area

# ------------------------ CÁLCULO DE INSUMOS ------------------------ #

def calcular_insumos(cultura, area):
    """
    Calcula a quantidade de insumos necessária para a cultura com base na área.

    Para Cana-de-Açúcar, usa dados reais:
      - Solicita o tipo de região para definir o espaçamento entre ruas:
            Região Montanhosa: 1.0 m
            Região Plana: 1.3 m (utilizado como padrão)
      - Calcula o lado aproximado do campo (assumindo formato quadrado), número de ruas (lado / espaçamento),
        comprimento total das ruas e a quantidade total de fertilizante NPK 12-06-12 (0,088 L/m²).
      - Calcula também a aplicação aproximada em mL por metro linear.

    Para Mandioca, utiliza uma taxa simbólica de 0,05 L/m² para insumo (fertilizante orgânico).

    Parâmetros:
        cultura (str): Nome da cultura.
        area (float): Área em m².

    Retorna:
        float: Quantidade total de insumo necessária.
    """
    print(colored("\n=== Cálculo de Insumos ===", color="yellow", style="bright")
          if COLORAMA_AVAILABLE else "\n=== Cálculo de Insumos ===")

    print(colored(f"Calculando insumos para {cultura}...", color="cyan")
          if COLORAMA_AVAILABLE else f"Calculando insumos para {cultura}...")
    time.sleep(0.7)

    if cultura.lower() == "cana de açúcar":
        print(colored("\nSelecione o tipo de região para o espaçamento entre ruas:", color="cyan")
              if COLORAMA_AVAILABLE else "\nSelecione o tipo de região para o espaçamento entre ruas:")
        print("1 - Região Montanhosa (espaçamento de 1.0 m)")
        print("2 - Região Plana (espaçamento de 1.3 m)")
        opc_regiao = input(colored("Opção: ", color="yellow") if COLORAMA_AVAILABLE else "Opção: ").strip()
        if opc_regiao == "1":
            spacing = 1.0
        else:
            spacing = 1.3

        # Assumindo que a área seja aproximadamente quadrada:
        lado = math.sqrt(area)
        # Número estimado de ruas considerando o lado do campo:
        num_ruas = round(lado / spacing)
        # Comprimento total das ruas (assumindo que cada rua tem comprimento igual ao lado do campo):
        total_rua_length = num_ruas * lado

        # Quantidade total de fertilizante (em litros)
        total_fert = round(area * 0.088, 2)
        # Aplicação por metro linear (convertendo para mL)
        app_per_meter = round(total_fert / total_rua_length, 3) if total_rua_length != 0 else 0

        print(colored("\n--- Cálculo para Cana-de-Açúcar ---", color="magenta", style="bright")
              if COLORAMA_AVAILABLE else "\n--- Cálculo para Cana-de-Açúcar ---")
        print(f"Área informada: {area:.2f} m²")
        print(f"Lado aproximado do campo: {lado:.2f} m")
        print(f"Número estimado de ruas: {num_ruas}")
        print(f"Comprimento total das ruas: {total_rua_length:.2f} m")
        print(f"Quantidade total de fertilizante NPK 12-06-12: {total_fert:.2f} L")
        print(f"Aplicação aproximada: {app_per_meter * 1000:.2f} mL por metro linear")

        return total_fert
    else:
        total_insumo = round(area * 0.05, 2)
        print(colored("\n--- Cálculo para Mandioca ---", color="magenta", style="bright")
              if COLORAMA_AVAILABLE else "\n--- Cálculo para Mandioca ---")
        print(f"Área informada: {area:.2f} m²")
        print(f"Quantidade total de insumo (fertilizante orgânico): {total_insumo:.2f} L")
        return total_insumo

# ------------------------ OPÇÕES DO MENU ------------------------- #

def gerar_id():
    """Gera um ID único para cada registro."""
    timestamp = int(time.time())
    random_part = random.randint(1000, 9999)
    return f"REG-{timestamp}-{random_part}"

def entrada_dados():
    """
    Realiza a inserção de um novo registro de dados para uma cultura.

    O usuário escolhe a cultura, insere os dados e o registro é armazenado na lista.
    """
    print(colored("\n=== Entrada de Dados ===", color="yellow", style="bright")
          if COLORAMA_AVAILABLE else "\n=== Entrada de Dados ===")

    print("Qual cultura deseja adicionar?")
    print(colored("1 - Cana de Açúcar", color="cyan") if COLORAMA_AVAILABLE else "1 - Cana de Açúcar")
    print(colored("2 - Mandioca", color="cyan") if COLORAMA_AVAILABLE else "2 - Mandioca")

    opc = input(colored("Opção: ", color="yellow") if COLORAMA_AVAILABLE else "Opção: ").strip()

    if opc == "1":
        cultura = "Cana de Açúcar"
        area = calcular_area_cana()
    elif opc == "2":
        cultura = "Mandioca"
        area = calcular_area_mandioca()
    else:
        print(colored("Opção inválida. Voltando ao menu.", color="red")
              if COLORAMA_AVAILABLE else "Opção inválida. Voltando ao menu.")
        return

    insumo = calcular_insumos(cultura, area)
    data_hora = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    registro_id = gerar_id()

    registro = {
        "id": registro_id,
        "cultura": cultura,
        "area": area,
        "insumo": insumo,
        "data_registro": data_hora,
        "custo_estimado": round(insumo * (150 if cultura == "Cana de Açúcar" else 90), 2)
    }

    cultura_dados.append(registro)
    show_loading("Salvando dados", 1.2)
    print(colored("Dados inseridos com sucesso!", color="green", style="bright")
          if COLORAMA_AVAILABLE else "Dados inseridos com sucesso!")

def atualizar_dados():
    """
    Atualiza os dados de um registro existente.

    Permite alterar a cultura ou recalcular a área e os insumos.
    """
    print(colored("\n=== Atualização de Dados ===", color="yellow", style="bright")
          if COLORAMA_AVAILABLE else "\n=== Atualização de Dados ===")

    if not cultura_dados:
        print(colored("Não há dados para atualizar.", color="red")
              if COLORAMA_AVAILABLE else "Não há dados para atualizar.")
        return

    saida_dados()
    try:
        index = int(input(colored("\nDigite o índice do registro que deseja atualizar: ", color="cyan")
                          if COLORAMA_AVAILABLE else "\nDigite o índice do registro que deseja atualizar: "))
    except ValueError:
        print(colored("Índice inválido. Retornando ao menu.", color="red")
              if COLORAMA_AVAILABLE else "Índice inválido. Retornando ao menu.")
        return

    if index < 0 or index >= len(cultura_dados):
        print(colored("Índice fora do intervalo. Retornando ao menu.", color="red")
              if COLORAMA_AVAILABLE else "Índice fora do intervalo. Retornando ao menu.")
        return

    registro = cultura_dados[index]

    if TABULATE_AVAILABLE:
        headers = ["Campo", "Valor"]
        data = [[k, v] for k, v in registro.items()]
        print("\nRegistro atual:")
        print(tabulate(data, headers=headers, tablefmt="fancy_grid"))
    else:
        print("\nRegistro atual:")
        for k, v in registro.items():
            print(f"{k}: {v}")

    print(colored("\nO que deseja atualizar?", color="yellow") if COLORAMA_AVAILABLE else "\nO que deseja atualizar?")
    print(colored("1 - Alterar Cultura", color="cyan") if COLORAMA_AVAILABLE else "1 - Alterar Cultura")
    print(colored("2 - Recalcular Área e Insumos", color="cyan") if COLORAMA_AVAILABLE else "2 - Recalcular Área e Insumos")
    print(colored("3 - Cancelar", color="cyan") if COLORAMA_AVAILABLE else "3 - Cancelar")

    opc = input(colored("Opção: ", color="yellow") if COLORAMA_AVAILABLE else "Opção: ").strip()

    if opc == "1":
        print(colored("Selecione a nova cultura:", color="yellow") if COLORAMA_AVAILABLE else "Selecione a nova cultura:")
        print(colored("1 - Cana de Açúcar", color="cyan") if COLORAMA_AVAILABLE else "1 - Cana de Açúcar")
        print(colored("2 - Mandioca", color="cyan") if COLORAMA_AVAILABLE else "2 - Mandioca")

        opc_cult = input(colored("Opção: ", color="yellow") if COLORAMA_AVAILABLE else "Opção: ").strip()

        if opc_cult == "1":
            registro["cultura"] = "Cana de Açúcar"
        elif opc_cult == "2":
            registro["cultura"] = "Mandioca"
        else:
            print(colored("Opção inválida, cancelando atualização.", color="red")
                  if COLORAMA_AVAILABLE else "Opção inválida, cancelando atualização.")
            return

        registro["insumo"] = calcular_insumos(registro["cultura"], registro["area"])
        registro["custo_estimado"] = round(registro["insumo"] * (150 if registro["cultura"] == "Cana de Açúcar" else 90), 2)
        registro["data_registro"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        show_loading("Atualizando registro", 1.0)
        print(colored("Cultura e insumo atualizados com sucesso!", color="green", style="bright")
              if COLORAMA_AVAILABLE else "Cultura e insumo atualizados com sucesso!")

    elif opc == "2":
        if registro["cultura"].lower() == "cana de açúcar":
            nova_area = calcular_area_cana()
        else:
            nova_area = calcular_area_mandioca()

        registro["area"] = nova_area
        registro["insumo"] = calcular_insumos(registro["cultura"], nova_area)
        registro["custo_estimado"] = round(registro["insumo"] * (150 if registro["cultura"] == "Cana de Açúcar" else 90), 2)
        registro["data_registro"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        show_loading("Atualizando registro", 1.0)
        print(colored("Área e insumos atualizados com sucesso!", color="green", style="bright")
              if COLORAMA_AVAILABLE else "Área e insumos atualizados com sucesso!")
    else:
        print(colored("Operação cancelada.", color="yellow") if COLORAMA_AVAILABLE else "Operação cancelada.")

def deletar_dados():
    """
    Remove um registro de dados existente.

    Solicita o índice do registro e, mediante confirmação, exclui o registro.
    """
    print(colored("\n=== Deleção de Dados ===", color="yellow", style="bright")
          if COLORAMA_AVAILABLE else "\n=== Deleção de Dados ===")

    if not cultura_dados:
        print(colored("Não há dados para deletar.", color="red")
              if COLORAMA_AVAILABLE else "Não há dados para deletar.")
        return

    saida_dados()
    try:
        index = int(input(colored("\nDigite o índice do registro que deseja deletar: ", color="cyan")
                          if COLORAMA_AVAILABLE else "\nDigite o índice do registro que deseja deletar: "))
    except ValueError:
        print(colored("Índice inválido. Retornando ao menu.", color="red")
              if COLORAMA_AVAILABLE else "Índice inválido. Retornando ao menu.")
        return

    if index < 0 or index >= len(cultura_dados):
        print(colored("Índice fora do intervalo. Retornando ao menu.", color="red")
              if COLORAMA_AVAILABLE else "Índice fora do intervalo. Retornando ao menu.")
        return

    registro = cultura_dados[index]

    if TABULATE_AVAILABLE:
        headers = ["Campo", "Valor"]
        data = [[k, v] for k, v in registro.items()]
        print("\nRegistro a ser deletado:")
        print(tabulate(data, headers=headers, tablefmt="fancy_grid"))
    else:
        print("\nRegistro a ser deletado:")
        for k, v in registro.items():
            print(f"{k}: {v}")

    confirm = input(colored("\nConfirma a deleção deste registro? (S/N): ", color="red")
                   if COLORAMA_AVAILABLE else "\nConfirma a deleção deste registro? (S/N): ").strip().upper()

    if confirm == "S":
        show_loading("Deletando registro", 1.0)
        cultura_dados.pop(index)
        print(colored("Registro deletado com sucesso!", color="green", style="bright")
              if COLORAMA_AVAILABLE else "Registro deletado com sucesso!")
    else:
        print(colored("Operação cancelada.", color="yellow") if COLORAMA_AVAILABLE else "Operação cancelada.")

def saida_dados():
    """
    Exibe de forma organizada todos os registros cadastrados.

    Caso não haja registros, informa o usuário.
    """
    print(colored("\n=== Saída de Dados ===", color="yellow", style="bright")
          if COLORAMA_AVAILABLE else "\n=== Saída de Dados ===")

    if not cultura_dados:
        print(colored("Não há dados para exibir.", color="red")
              if COLORAMA_AVAILABLE else "Não há dados para exibir.")
        return

    if TABULATE_AVAILABLE:
        dados_para_exibir = []
        for idx, reg in enumerate(cultura_dados):
            dados_para_exibir.append([
                idx,
                reg['cultura'],
                f"{reg['area']:.2f} m²",
                f"{reg['insumo']:.2f} L",
                f"R$ {reg['custo_estimado']:.2f}",
                reg['data_registro']
            ])

        headers = ["Índice", "Cultura", "Área", "Insumo", "Custo Est.", "Data Registro"]
        print(tabulate(dados_para_exibir, headers=headers, tablefmt="fancy_grid"))
    else:
        print("-" * 80)
        for idx, reg in enumerate(cultura_dados):
            print(f"[{idx}] Cultura: {reg['cultura']}, "
                  f"Área: {reg['area']:.2f} m², "
                  f"Insumo: {reg['insumo']:.2f} L, "
                  f"Custo Est.: R$ {reg['custo_estimado']:.2f}, "
                  f"Data: {reg['data_registro']}")
        print("-" * 80)

    if len(cultura_dados) > 0:
        print(colored("\n=== Estatísticas Básicas ===", color="cyan")
              if COLORAMA_AVAILABLE else "\n=== Estatísticas Básicas ===")

        total_area = sum(reg['area'] for reg in cultura_dados)
        total_insumo = sum(reg['insumo'] for reg in cultura_dados)
        total_custo = sum(reg['custo_estimado'] for reg in cultura_dados)

        print(f"Total de registros: {len(cultura_dados)}")
        print(f"Área total: {total_area:.2f} m²")
        print(f"Total de insumos: {total_insumo:.2f} L")
        print(f"Custo total estimado: R$ {total_custo:.2f}")

def exportar_dados():
    """
    Exporta os dados cadastrados para um arquivo CSV.

    O arquivo será usado para análise posterior em R.
    """
    print(colored("\n=== Exportar Dados para CSV ===", color="yellow", style="bright")
          if COLORAMA_AVAILABLE else "\n=== Exportar Dados para CSV ===")

    if not cultura_dados:
        print(colored("Não há dados para exportar.", color="red")
              if COLORAMA_AVAILABLE else "Não há dados para exportar.")
        return

    nome_arquivo = input(colored("Digite o nome do arquivo (sem extensão): ", color="cyan")
                         if COLORAMA_AVAILABLE else "Digite o nome do arquivo (sem extensão): ").strip() or "dados_culturas"

    caminho_r = r"C:\Users\USUARIO\Desktop\FIAP\FarmTech\r_app\Dados para Estudo via R"

    if not os.path.exists(caminho_r):
        try:
            os.makedirs(caminho_r)
            print(colored(f"Diretório {caminho_r} criado com sucesso.", color="green")
                  if COLORAMA_AVAILABLE else f"Diretório {caminho_r} criado com sucesso.")
        except Exception as e:
            print(colored(f"Não foi possível criar o diretório: {e}", color="red")
                  if COLORAMA_AVAILABLE else f"Não foi possível criar o diretório: {e}")
            caminho_r = "."

    nome_arquivo_completo = os.path.join(caminho_r, f"{nome_arquivo}.csv")

    try:
        show_loading("Preparando exportação", 1.0)

        with open(nome_arquivo_completo, 'w', newline='', encoding='utf-8') as arquivo:  # Adicionado encoding='utf-8'
            campos = ['cultura', 'area', 'insumo', 'custo_estimado', 'data_registro']
            writer = csv.DictWriter(arquivo, fieldnames=campos)
            writer.writeheader()

            for registro in cultura_dados:
                reg_exportar = {
                    'cultura': registro['cultura'],
                    'area': round(registro['area'], 2),
                    'insumo': round(registro['insumo'], 2),
                    'custo_estimado': round(registro['custo_estimado'], 2),
                    'data_registro': registro['data_registro']
                }
                writer.writerow(reg_exportar)

        show_loading("Finalizando exportação", 0.8)

        print(colored(f"\nDados exportados com sucesso para '{nome_arquivo_completo}'!", color="green", style="bright")
              if COLORAMA_AVAILABLE else f"\nDados exportados com sucesso para '{nome_arquivo_completo}'!")

        print(colored("\n=== Preview do CSV Exportado ===", color="cyan")
              if COLORAMA_AVAILABLE else "\n=== Preview do CSV Exportado ===")

        with open(nome_arquivo_completo, 'r', encoding='utf-8') as arquivo: # encoding='utf-8' para preview
            linhas = arquivo.readlines()[:min(5, len(cultura_dados) + 1)]
            for linha in linhas:
                print(colored(linha.strip(), color="white") if COLORAMA_AVAILABLE else linha.strip())

            if len(cultura_dados) > 4:
                print(colored("...", color="white") if COLORAMA_AVAILABLE else "...")

        print(colored(f"\nVocê pode usar este arquivo com o script R para análises estatísticas.", color="yellow")
              if COLORAMA_AVAILABLE else f"\nVocê pode usar este arquivo com o script R para análises estatísticas.")
        print(colored(f"Caminho do arquivo: {nome_arquivo_completo}", color="yellow")
              if COLORAMA_AVAILABLE else f"Caminho do arquivo: {nome_arquivo_completo}")

    except Exception as e:
        print(colored(f"Erro ao exportar dados: {e}", color="red")
              if COLORAMA_AVAILABLE else f"Erro ao exportar dados: {e}")

def menu():
    """
    Exibe o menu principal da aplicação e gerencia as opções do usuário.

    Permite navegar pelas funcionalidades de inserção, atualização, deleção e exibição de dados.
    """
    while True:
        clear_screen()
        print_ascii_banner()

        opcoes = [
            (1, "Entrada de dados", "green"),
            (2, "Saída de dados", "blue"),
            (3, "Atualização de dados", "yellow"),
            (4, "Deleção de dados", "red"),
            (5, "Exportar para CSV", "magenta"),
            (6, "Sair do programa", "white")
        ]

        if COLORAMA_AVAILABLE:
            for num, texto, cor in opcoes:
                print(f" {colored(f'[{num}]', color=cor)} {colored(texto, color=cor)}")
        else:
            for num, texto, _ in opcoes:
                print(f" [{num}] {texto}")

        print("=" * 60)

        opc = input(colored("Escolha uma opção: ", color="yellow", style="bright")
                    if COLORAMA_AVAILABLE else "Escolha uma opção: ").strip()

        if opc == "1":
            entrada_dados()
        elif opc == "2":
            saida_dados()
        elif opc == "3":
            atualizar_dados()
        elif opc == "4":
            deletar_dados()
        elif opc == "5":
            exportar_dados()
        elif opc == "6":
            print(colored("\nSaindo do programa... Obrigado por utilizar o FarmTech Solutions!", color="cyan", style="bright")
                  if COLORAMA_AVAILABLE else "\nSaindo do programa... Obrigado por utilizar o FarmTech Solutions!")
            show_loading("Finalizando", 1.0)
            break
        else:
            print(colored("Opção inválida, tente novamente.", color="red")
                  if COLORAMA_AVAILABLE else "Opção inválida, tente novamente.")

        input(colored("\nPressione Enter para continuar...", color="yellow")
              if COLORAMA_AVAILABLE else "\nPressione Enter para continuar...")

if __name__ == "__main__":
    clear_screen()

    if COLORAMA_AVAILABLE:
        print(colored("Iniciando FarmTech Solutions...", color="cyan", style="bright"))
    else:
        print("Iniciando FarmTech Solutions...")

    show_loading("Carregando módulos", 1.5)
    show_loading("Inicializando interface", 1.0)

    menu()
