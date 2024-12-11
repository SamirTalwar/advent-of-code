namespace AdventOfCode2024;

class Day09Part1
{
    public static void Run(string[] args)
    {
        var chars = Input.String().ToCharArray();
        var currentOffset = 0;
        var currentBlockId = 0;
        var blocks = new List<BlockInfo>();
        var freeSpace = new List<FreeSpace>();
        for (var i = 0; i < chars.Length; i += 2)
        {
            var blockSize = chars[i] - '0';
            blocks.Add(new BlockInfo
            {
                Id = currentBlockId,
                Offset = currentOffset,
                Size = blockSize,
            });
            currentBlockId += 1;
            currentOffset += blockSize;

            if (i + 1 < chars.Length)
            {
                var freeSpaceSize = chars[i + 1] - '0';
                freeSpace.Add(new FreeSpace
                {
                    Offset = currentOffset,
                    Size = freeSpaceSize,
                });
                currentOffset += freeSpaceSize;
            }
        }

        while (true)
        {
            var firstFreeSpace = freeSpace[0];
            var lastBlock = blocks[^1];
            if (firstFreeSpace.Offset >= lastBlock.Offset + lastBlock.Size)
            {
                break;
            }

            if (firstFreeSpace.Size > lastBlock.Size)
            {
                blocks.RemoveRange(blocks.Count - 1, 1);
                lastBlock.Offset = firstFreeSpace.Offset;
                var index = blocks.FindIndex(b => b.Offset > firstFreeSpace.Offset);
                if (index >= 0)
                {
                    blocks.Insert(index, lastBlock);
                }
                else
                {
                    blocks.Add(lastBlock);
                }
                firstFreeSpace.Offset += lastBlock.Size;
                firstFreeSpace.Size -= lastBlock.Size;
            }
            else
            {
                freeSpace.RemoveRange(0, 1);
                var newBlock = new BlockInfo
                {
                    Id = lastBlock.Id,
                    Offset = firstFreeSpace.Offset,
                    Size = firstFreeSpace.Size,
                };
                var index = blocks.FindIndex(b => b.Offset > firstFreeSpace.Offset);
                if (index >= 0)
                {
                    blocks.Insert(index, newBlock);
                }
                else
                {
                    blocks.Add(newBlock);
                }
                lastBlock.Size -= firstFreeSpace.Size;
                if (lastBlock.Size == 0)
                {
                    blocks.RemoveRange(blocks.Count - 1, 1);
                }
            }
        }

        var result = blocks.SelectMany(block => Enumerable.Range(block.Offset, block.Size).Select(i => i * block.Id)).Sum();

        Console.WriteLine("{0}", result);
    }

    record BlockInfo
    {
        public long Id { get; init; }
        public int Offset { get; set; }
        public int Size { get; set; }

        public override string ToString() => $"{{id {Id}, offset {Offset}, size {Size}}}";
    }

    record FreeSpace
    {
        public int Offset { get; set; }
        public int Size { get; set; }

        public override string ToString() => $"{{offset {Offset}, size {Size}}}";
    }
}
